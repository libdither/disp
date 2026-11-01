// The block frame's viewer: runs the design as Bend built it (out/view.mjs, from view.bend) on a program and a
// tree, draws the grid tick by tick with the blocks the next tick acts on, and shows LAWS.bend with the last
// check beside it. It reloads itself whenever view.sh or check.sh finishes. Bundled into ../block.js by bundle.mts.
import { parseProg, parseVal, valText } from '../../../website/src/lib/evaluators/twoway/text.ts'
import { F, L, S, type V } from '../../../website/src/lib/evaluators/twoway/core.ts'
import { LEGEND, describe, draw as drawCell } from './skin.ts'

const $ = <T extends HTMLElement = HTMLElement>(id: string) => document.getElementById(id) as T
const cv = $<HTMLCanvasElement>('cv')
const ctx = cv.getContext('2d')!

// ---- Bend values: {$: constructor, ...fields}, Nat a number, Bool a boolean ----
type B = { $: string; [field: string]: any }
interface View {
  fields(): B; reactions(): number; rule(): B; start(p: B, x: B): B; tick(idx: B, g: B, shifted: boolean): B
  read(g: B, port: number): B; moved(u: B, t: B): boolean; reference(p: B, x: B): B
}
const toB = (v: V): B => (v.tag === 'leaf' ? { $: 'L' } : v.tag === 'stem' ? { $: 'S', c: toB(v.c) } : { $: 'F', l: toB(v.l), r: toB(v.r) })
const fromB = (b: B): V => (b.$ === 'L' ? L() : b.$ === 'S' ? S(fromB(b.c)) : F(fromB(b.l), fromB(b.r)))
function list(b: B): any[] {
  const out: any[] = []
  for (; b.$ === 'Con'; b = b.tail) out.push(b.head)
  return out
}
const maybe = (b: B): B | null => (b.$ === 'Some' ? b.value : null)
const treeText = (b: B | null) => (b ? valText(fromB(b), 'tree') : 'none')
const same = (a: B | null, b: B | null) => (a && b ? treeText(a) === treeText(b) : a === b)

// ---- the run ----
type Cells = number[][][]
interface Frame { g: B; cells: Cells; reads: B | null; outs: number; moved: boolean; sealed: boolean }
interface Run { frames: Frame[]; port: number; still: boolean; answer: B | null; reference: B | null; outside: boolean; left: number }
const CAP = 4000
let view: View | null = null
let rule: B | null = null
let run: Run | null = null
let t = 0

const cellsOf = (g: B): Cells => list(g).map((row: B) => list(row).map((c: B) => list(c) as number[]))
const changed = (a: Cells, b: Cells) => JSON.stringify(a) !== JSON.stringify(b)

function compute(p: V, x: V): Run {
  const w = view!, pb = toB(p), xb = toB(x)
  const reference = maybe(w.reference(pb, xb))
  const st = maybe(w.start(pb, xb))
  if (!st) return { frames: [], port: 0, still: false, answer: null, reference, outside: true, left: 0 }
  const port = st.port as number
  let g = st.g
  const frames: Frame[] = [{ g, cells: cellsOf(g), reads: maybe(w.read(g, port)), outs: 0, moved: true, sealed: true }]
  let quiet = 0, left = 0
  for (let k = 0; k < CAP && quiet < 2; k++) {
    const cut = w.tick(rule!, g, k % 2 === 1)
    const outs = list(cut.outs)
    g = cut.g
    const prev = frames[frames.length - 1], cells = cellsOf(g), reads = maybe(w.read(g, port))
    quiet = !changed(prev.cells, cells) && !outs.length ? quiet + 1 : 0
    left += outs.length
    frames.push({ g, cells, reads, outs: outs.length, moved: !reads || !prev.reads || w.moved(reads, prev.reads), sealed: outs.every((o: B) => o.x === port + 1 && o.y === 0) })
  }
  const still = quiet >= 2
  return { frames, port, still, answer: still ? frames[frames.length - 1].reads : null, reference, outside: false, left }
}

// ---- inputs ----
const marked = '((△△, △), (△, △))'
const PRESETS: [string, string, string?][] = [
  ['swap', marked],
  ['swap ; swap', marked],
  ['swap†', '(△, △△)'],
  ['first(swap)', marked],
  ['second(swap)', marked],
  ['first(swap) ; swap', marked],
  ['map(swap, swap, swap)', marked],
  ['map(swap, swap, swap)', '△(△△, △)', 'map on a stem: the swap goes into its part'],
  ['map(map(swap, id, id), swap, second(swap))', marked],
  ['second(first(swap)) ; swap', '((△, △△), ((△, △△), △))'],
  ['stem', marked],
  ['second(stem) ; swap', marked],
  ['stem†', '(△, △)', 'stem† on a fork: stuck'],
  ['unit', marked],
  ['unit ; first(stem)', marked],
  ['stem ; stem ; stem', marked, 'a third stem at the root: out of room, never goes still (the calculus has an answer)'],
  ['cross', '((△, △△), △)', 'cross: not in the design yet']
]

function rerun(keep = false) {
  $('err').textContent = ''
  if (!view) return
  let p: V, x: V
  try {
    p = parseProg($<HTMLInputElement>('prog').value)
    x = parseVal($<HTMLInputElement>('tree').value)
  } catch (e) {
    $('err').textContent = (e as Error).message
    return
  }
  history.replaceState(null, '', '?' + new URLSearchParams({ p: $<HTMLInputElement>('prog').value, v: $<HTMLInputElement>('tree').value }))
  const fresh = !run || !keep
  run = compute(p, x)
  t = keep ? Math.min(t, Math.max(0, run.frames.length - 1)) : 0
  $<HTMLInputElement>('time').max = String(Math.max(0, run.frames.length - 1))
  summary()
  if (fresh) fit()
  show()
}

function summary() {
  const r = run!, v = $('verdict')
  if (r.outside) {
    v.className = ''
    v.innerHTML = `outside the design: it does not run this program; the calculus gives <b>${treeText(r.reference)}</b>`
    $('stats').textContent = ''
    return
  }
  const stuck = !r.reference
  const ok = stuck ? null : r.still && same(r.answer, r.reference)
  v.className = ok === null ? '' : ok ? 'ok' : 'bad'
  v.innerHTML = stuck
    ? `the calculus is stuck here; the design ${r.still ? `ends reading <b>${treeText(r.answer)}</b>` : 'does not go still'}`
    : ok
      ? `✓ the calculus's answer, <b>${treeText(r.answer)}</b>`
      : `✗ the design gives <b>${r.still ? treeText(r.answer) : 'no still grid'}</b> but the calculus <b>${treeText(r.reference)}</b>`
  const f = r.frames, rows = f[0].cells.length, cols = f[0].cells[0]?.length ?? 0
  const bad = [f.some((x) => !x.reads) && 'a tick did not read as a tree', f.some((x) => !x.moved) && 'a tick changed the tree by more than moves', f.some((x) => !x.sealed) && 'something left away from the port']
    .filter(Boolean)
  $('stats').innerHTML =
    (r.still ? `still after <b>${f.length - 3}</b> ticks` : `<span class="bad">not still after ${CAP} ticks</span>`) +
    ` · grid ${cols}×${rows} · ${r.left} cells left through the port · ${view!.reactions()} reactions` +
    (bad.length ? ` · <span class="bad">${bad.join('; ')}</span>` : ' · every tick read as a tree and changed it only by moves')
  for (const id of ['verdict', 'stats']) $(id).title = $(id).textContent!
}

function show() {
  const r = run!
  $<HTMLInputElement>('time').value = String(t)
  $('tick').textContent = r.outside ? '' : `tick ${t} / ${r.frames.length - 1}`
  const fr = r.frames[t]
  $('now').innerHTML = fr ? `the grid reads <b>${treeText(fr.reads)}</b> at tick ${t}${fr.outs ? ` · ${fr.outs} cell left` : ''} · next tick: blocks at ${t % 2 ? 'odd' : 'even'} corners` : ''
  $('now').title = $('now').textContent!
  draw()
}

// ---- drawing ----
let z = 24, ox = 0, oy = 0
function fit() {
  const r = run!
  if (r.outside) return
  const g = r.frames[0].cells, rows = g.length, cols = g[0]?.length ?? 1
  const top = $('hud').offsetHeight + 20, bottom = $('dock').offsetHeight + 10, right = $('side').offsetWidth + 24
  const w = innerWidth - right - 24, h = innerHeight - top - bottom
  z = Math.max(4, Math.min(40, Math.min(w / cols, h / rows)))
  ox = 12 + (w - z * cols) / 2
  oy = top + (h - z * rows) / 2
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
  if (!run || run.outside) return
  const g = run.frames[t].cells, prev = t > 0 ? run.frames[t - 1].cells : null
  const rows = g.length, cols = g[0]?.length ?? 0
  // the blocks the next tick acts on
  const off = t % 2
  ctx.strokeStyle = '#161e2c'
  ctx.lineWidth = 1
  for (let y = off - 2; y < rows + 1; y += 2) for (let x = off - 2; x < cols + 1; x += 2) ctx.strokeRect(ox + x * z + 0.5, oy + y * z + 0.5, 2 * z - 1, 2 * z - 1)
  ctx.strokeStyle = '#2a3a52'
  ctx.strokeRect(ox, oy, cols * z, rows * z)
  // the port, where the program goes in and its echoes leave
  ctx.fillStyle = '#56c8ff'
  ctx.beginPath()
  const px = ox + (run.port + 0.5) * z
  ctx.moveTo(px, oy - 2)
  ctx.lineTo(px - z * 0.25, oy - z * 0.4)
  ctx.lineTo(px + z * 0.25, oy - z * 0.4)
  ctx.fill()
  g.forEach((row, y) => row.forEach((c, x) => {
    if (prev && JSON.stringify(prev[y]?.[x]) !== JSON.stringify(c)) {
      ctx.fillStyle = 'rgba(255,216,102,.10)'
      ctx.fillRect(ox + x * z, oy + y * z, z, z)
    }
    drawCell(ctx, c, ox + (x + 0.5) * z, oy + (y + 0.5) * z, z, (dx, dy) => g[y + dy]?.[x + dx] ?? null)
  }))
}

// ---- the side panel: laws, the last check ----
type Item = { kind: 'section' | 'note'; text: string } | { kind: 'law'; name: string; doc: string; small: boolean; body: string }
function parseLaws(src: string): Item[] {
  const items: Item[] = []
  let doc: string[] = [], first = true
  const flush = () => {
    if (doc.length && !first) {
      // a heading is the line over a line of dashes; what follows it is a note
      const d = doc.findIndex((l) => /^-+$/.test(l))
      if (d > 0) items.push({ kind: 'section', text: doc[d - 1] })
      const rest = d > 0 ? doc.slice(d + 1) : doc
      if (rest.length) items.push({ kind: 'note', text: rest.join(' ') })
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
    else if (/^def /.test(ln)) doc = []
  }
  flush()
  return items
}

const esc = (s: string) => s.replace(/[&<>]/g, (c) => ({ '&': '&amp;', '<': '&lt;', '>': '&gt;' })[c]!)
const emph = (s: string) => esc(s).replace(/(Restricts:|Leaves free:)/g, '<b>$1</b>')
async function fetchText(path: string): Promise<{ text: string; at: string } | null> {
  const r = await fetch(path, { cache: 'no-store' })
  if (!r.ok) return null
  const at = r.headers.get('Last-Modified')
  return { text: await r.text(), at: at ? new Date(at).toLocaleTimeString() : '' }
}

async function side() {
  const [laws, proofs] = await Promise.all(['block/LAWS.bend', 'block/out/proofs.txt'].map(fetchText))
  // check.sh empties the file while the proofs run
  const all = proofs?.text.trim() ? /ALL PROOFS CHECK/.test(proofs.text) : null
  const failed = proofs?.text.match(/Location: Laws\.(\w+)/)?.[1]
  const time = proofs?.text.match(/proofs: (\d+)s/)?.[1]
  let html = `<div class="hd">Laws <span>block/LAWS.bend, proved by PROOF.bend</span></div>`
  html += `<p class="${all ? 'g' : all === false ? 'bad' : ''}">${all === null ? (proofs ? 'the proofs are running…' : 'not checked yet: run block/check.sh') : all ? `every law holds (checked ${proofs!.at}${time ? `, ${time} s` : ''})` : `a proof fails (checked ${proofs!.at})`}</p>`
  let passed = true
  for (const it of laws ? parseLaws(laws.text) : []) {
    if (it.kind === 'section') html += `<div class="sec">${esc(it.text)}</div>`
    else if (it.kind === 'note') html += `<p class="note">${emph(it.text)}</p>`
    else {
      // Bend stops at the first failing proof: the laws before it hold, the ones after are unknown
      if (failed === it.name) passed = false
      const state = all ? 'ok' : failed === it.name ? 'fail' : passed && all === false ? 'ok' : ''
      const mark = state === 'ok' ? '✓' : state === 'fail' ? '✗' : '?'
      html += `<div class="law ${state}"><div class="top"><b>${it.name}</b>${it.small ? '<span class="tag">small</span>' : ''}<span class="mark">${mark}</span></div><div>${emph(it.doc)}</div><pre>${esc(it.body)}</pre></div>`
    }
  }
  $('laws').innerHTML = html
}

// ---- reloading on each build ----
let stamp = ''
const modified = async (path: string) => (await fetch(path, { method: 'HEAD', cache: 'no-store' })).headers.get('Last-Modified')
const ui = modified('block.js')
async function poll() {
  try {
    const r = await fetch('block/out/stamp', { cache: 'no-store' })
    const s = r.ok ? (await r.text()).trim() : ''
    if (!s) $('err').textContent = 'no build yet: run research/signal-fabric/block/view.sh'
    else if (s !== stamp) {
      const firstLoad = !stamp
      if (!firstLoad && (await modified('block.js')) !== (await ui)) return location.reload()
      stamp = s
      view = (await import(/* @vite-ignore */ new URL(`block/out/view.mjs?v=${encodeURIComponent(s)}`, location.href).href)).default as View
      rule = view.rule()
      $('cells').textContent = String(list(view.fields()).reduce((a: number, b: number) => a * b, 1))
      await side()
      rerun(!firstLoad)
      $('built').textContent = `built ${new Date(s).toLocaleTimeString()}`
    }
  } catch (e) {
    $('err').textContent = String((e as Error).message ?? e)
  }
  setTimeout(poll, 1500)
}

// ---- controls ----
let playing = false, last = 0
function play(on = !playing) {
  playing = on && !!run && !run.outside
  $('play').textContent = playing ? '❚❚' : '▶'
  if (playing && t >= run!.frames.length - 1) t = 0
  last = performance.now()
  if (playing) requestAnimationFrame(frame)
}
function frame(now: number) {
  if (!playing || !run) return
  if (now - last >= 1000 / Number($<HTMLSelectElement>('speed').value)) {
    last = now
    if (t >= run.frames.length - 1) return play(false)
    t++
    show()
  }
  requestAnimationFrame(frame)
}
const go = (to: number) => {
  if (!run || run.outside) return
  t = Math.max(0, Math.min(run.frames.length - 1, to))
  show()
}

$('legend').innerHTML = LEGEND
const sel = $<HTMLSelectElement>('preset')
PRESETS.forEach(([p, v, name], i) => sel.add(new Option(name ?? `${p}  on  ${v}`, String(i))))
sel.onchange = () => {
  const [p, v] = PRESETS[Number(sel.value)]
  $<HTMLInputElement>('prog').value = p
  $<HTMLInputElement>('tree').value = v
  rerun()
}
const params = new URLSearchParams(location.search)
$<HTMLInputElement>('prog').value = params.get('p') ?? PRESETS[6][0]
$<HTMLInputElement>('tree').value = params.get('v') ?? PRESETS[6][1]
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

// pan, zoom, and a cell's fields on hover
let drag: [number, number] | null = null
cv.onmousedown = (e) => (drag = [e.clientX - ox, e.clientY - oy])
addEventListener('mouseup', () => (drag = null))
cv.ondblclick = () => run && (fit(), draw())
cv.onwheel = (e) => {
  e.preventDefault()
  const k = Math.exp(-e.deltaY * 0.0015), nz = Math.max(2, Math.min(90, z * k))
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
  const c = run && !run.outside ? run.frames[t].cells[y]?.[x] : undefined
  if (!c) {
    tip.style.display = 'none'
    return
  }
  tip.textContent = `(${x}, ${y}) ${describe(c)}`
  tip.style.display = 'block'
  tip.style.left = `${e.clientX + 14}px`
  tip.style.top = `${e.clientY + 14}px`
}
cv.onmouseleave = () => ($('tip').style.display = 'none')

poll()
