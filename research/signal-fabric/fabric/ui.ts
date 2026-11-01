// The player: compiles the chosen program, lays it out on the chip, and animates set-up, runs and flips
// on a canvas. Bundled into ../fabric.js by bundle.mts.
import { valText } from '../../../website/src/lib/evaluators/twoway/text.ts'
import { eq, type V } from '../../../website/src/lib/evaluators/twoway/core.ts'
import { EXAMPLES } from './examples.ts'
import { circuitOf, bitsOf, withBits, outputValue, answer, type Circuit } from './circuit.ts'
import { layout, DX, DY, opp, cellAcross, type Layout } from './layout.ts'
import { Chip, simulate, retract, setup, type Knobs, type Run } from './sim.ts'

const $ = <T extends HTMLElement = HTMLElement>(id: string) => document.getElementById(id) as T
const cv = $<HTMLCanvasElement>('cv')
const ctx = cv.getContext('2d')!
const COLORS = { one: '#ffd866', zero: '#4f86c6', idle: '#253246', wrong: '#ff6b6b', noise: '#ff5fd2', heat: '#ff9e64', clock: '#b39cff', wave: '#56c8ff' }

// ---- state ----
const knobs: Knobs = { vdd: 0.28, c: 1e-15, adiabatic: false, ramp: 5e-10, test: true, seed: 1 }
let circuit: Circuit | null = null
let bits: number[] = []
let chip: Chip
let lay: Layout | null = null
let su: ReturnType<typeof setup> | null = null
let run: Run | null = null // the run shown
let shown: Run | null = null // the values on the wires before it (static clocking holds them)
let charged = false // adiabatic: the levels are up
let back: ReturnType<typeof retract> | null = null
let drained = 0 // static flip: heat of draining
type Anim = { kind: 'load' | 'run' | 'flip'; start: number; dur: number; phys: number }
let anim: Anim | null = null
let loaded = false
let noiseSeed = 99
const rand = () => ((noiseSeed = (Math.imul(noiseSeed, 1664525) + 1013904223) >>> 0) / 4294967296)

// ---- formatting ----
const fmtT = (t: number) => (t < 1e-9 ? `${(t * 1e12).toPrecision(3)} ps` : t < 1e-6 ? `${(t * 1e9).toPrecision(3)} ns` : t < 1e-3 ? `${(t * 1e6).toPrecision(3)} µs` : `${(t * 1e3).toPrecision(3)} ms`)
const fmtE = (e: number) => (e < 1e-15 ? `${(e * 1e18).toPrecision(3)} aJ` : e < 1e-12 ? `${(e * 1e15).toPrecision(3)} fJ` : e < 1e-9 ? `${(e * 1e12).toPrecision(3)} pJ` : `${(e * 1e9).toPrecision(3)} nJ`)
const fmtP = (p: number) => (p <= 0 ? '0' : p < 1e-3 || p >= 1e3 ? p.toExponential(0).replace('e', '×10^').replace('+', '') : p.toPrecision(2))
const reads = () => (circuit?.regs ? 'regs' : 'bool')
const show = (v: V | null) => (v ? valText(v, reads()) : 'stuck')

// ---- compile and lay out ----
function compileNow() {
  const prog = $<HTMLInputElement>('prog').value.trim()
  const inp = $<HTMLInputElement>('inp').value.trim()
  try {
    circuit = circuitOf(prog, inp)
    bits = bitsOf(circuit.input, circuit.regs)
    $('err').textContent = circuit.stuck ? 'note: the program gets stuck on some inputs; those give no answer' : ''
  } catch (e) {
    circuit = null
    lay = null
    $('err').textContent = (e as Error).message
    return
  }
  build(true)
}
function build(refit: boolean) {
  if (!circuit) return
  chip = new Chip({ ...knobs })
  try {
    lay = layout(circuit, knobs.test ? (x, y) => chip.faulty(x, y) : () => false)
  } catch (e) {
    lay = null
    $('err').textContent = `${(e as Error).message}: raise the supply or the cell size`
    hud()
    return
  }
  su = setup(lay, chip)
  geometry()
  run = shown = null
  back = null
  charged = false
  loaded = false
  if (refit) fit()
  play('load')
}

// ---- actions ----
const speed = () => 2 ** +$<HTMLInputElement>('speed').value
function play(kind: Anim['kind']) {
  if (!lay || !su || !circuit) return
  if (kind === 'load') anim = { kind, start: performance.now(), dur: 3200 / speed(), phys: su.time }
  else if (kind === 'run') anim = { kind, start: performance.now(), dur: 2600 / speed(), phys: knobs.adiabatic ? run!.time : run!.settle }
  else anim = { kind, start: performance.now(), dur: (knobs.adiabatic ? 2600 : 900) / speed(), phys: knobs.adiabatic ? run!.time : 1 }
  story()
}
function doRun(zap: number | null = null) {
  if (!lay || !circuit) return
  if (!loaded) { play('load'); return }
  if (knobs.adiabatic && charged && run) back = retract(circuit, lay, chip, run, bits) // the old levels come down first
  shown = !knobs.adiabatic && charged ? run : null
  run = simulate(circuit, lay, chip, bits, shown, zap, rand)
  back = null
  drained = 0
  charged = true
  play('run')
}
function doFlip() {
  if (!run || !charged || !lay || !circuit) return
  if (knobs.adiabatic) {
    back = retract(circuit, lay, chip, run, bits)
    back.heat += run.heat.leak // ramping down takes as long as ramping up
  } else drained = run.charged
  charged = false
  play('flip')
}
function doZap() {
  if (!lay) return
  const cells = [...new Set(lay.routes.flatMap((r) => r.links.map((l) => l >> 2)))].filter((c) => lay!.sites.get(c)?.k !== 'in')
  if (cells.length) doRun(cells[Math.floor(Math.random() * cells.length)])
}

// ---- geometry: each link drawn as the stretch of wire inside the cell it leaves, plus the stub into a gate or pad ----
const S = 20
let paths: (Path2D | null)[] = []
let cellStage = new Int32Array(0)
const center = (cell: number) => [((cell % lay!.w) + 0.5) * S, (Math.floor(cell / lay!.w) + 0.5) * S]
const TAN = [[1, 0], [0, 1], [-1, 0], [0, -1]]
const LANE = S * 0.17
const BOX = S * 0.3
function sidePoint(cell: number, side: number, out: boolean): [number, number] {
  const [cx, cy] = center(cell)
  const s = out ? -LANE : LANE
  return [cx + DX[side] * S * 0.5 + TAN[side][0] * s, cy + DY[side] * S * 0.5 + TAN[side][1] * s]
}
function geometry() {
  const l0 = lay!
  paths = new Array(l0.w * l0.h * 4).fill(null)
  cellStage = new Int32Array(l0.w * l0.h).fill(-1)
  const level = new Map<number, number>()
  circuit!.gates.forEach((g) => level.set(g.net, g.level))
  for (const r of l0.routes) {
    const inSide = new Map<number, number>()
    const st = level.get(r.net) ?? 0
    for (const l of r.links) {
      const u = l >> 2, d = l & 3
      const p = new Path2D()
      const [ox, oy] = sidePoint(u, d, true)
      if (u === r.src) {
        const [cx, cy] = center(u)
        p.moveTo(cx + DX[d] * BOX + TAN[d][0] * -LANE, cy + DY[d] * BOX + TAN[d][1] * -LANE)
        p.lineTo(ox, oy)
      } else {
        const s = inSide.get(u)!
        const [ix, iy] = sidePoint(u, s, false)
        p.moveTo(ix, iy)
        if (s === opp(d)) p.lineTo(ox, oy)
        else if (DX[s] !== 0) p.quadraticCurveTo(ox, iy, ox, oy)
        else p.quadraticCurveTo(ix, oy, ox, oy)
      }
      const v = cellAcross(l, l0.w)
      inSide.set(v, opp(d))
      if (l0.sites.has(v)) {
        const [ix, iy] = sidePoint(v, opp(d), false)
        const [cx, cy] = center(v)
        p.moveTo(ix, iy)
        p.lineTo(cx - DX[d] * BOX + TAN[opp(d)][0] * LANE, cy - DY[d] * BOX + TAN[opp(d)][1] * LANE)
      }
      paths[l] = p
      if (cellStage[u] < 0 || !l0.sites.has(u)) cellStage[u] = Math.max(cellStage[u], st)
    }
  }
  circuit!.gates.forEach((g, i) => (cellStage[l0.gateAt[i]] = g.level))
}

// ---- view ----
let scale = 1, ox = 0, oy = 0
function fit() {
  if (!lay) return
  const W = cv.clientWidth, H = cv.clientHeight
  const top = 190, bottom = 130, side = 90
  scale = Math.min((W - 2 * side - 270) / (lay.w * S), (H - top - bottom) / (lay.h * S), 3)
  ox = side + (W - 270 - 2 * side - lay.w * S * scale) / 2
  oy = top + (H - top - bottom - lay.h * S * scale) / 2
}

// ---- drawing ----
function mix(a: string, b: string, t: number) {
  const pa = [1, 3, 5].map((i) => parseInt(a.slice(i, i + 2), 16))
  const pb = [1, 3, 5].map((i) => parseInt(b.slice(i, i + 2), 16))
  return `rgb(${pa.map((x, i) => Math.round(x + (pb[i] - x) * Math.max(0, Math.min(1, t)))).join(',')})`
}
/** where the animation is, in the physics' own time */
function now(): { kind: Anim['kind'] | 'idle'; tau: number; p: number } {
  if (!anim) return { kind: 'idle', tau: 0, p: 1 }
  const p = Math.min(1, (performance.now() - anim.start) / anim.dur)
  if (p >= 1) {
    const k = anim.kind
    anim = null
    if (k === 'load') { loaded = true; doRun() }
    story()
    hud()
    return { kind: 'idle', tau: 0, p: 1 }
  }
  return { kind: anim.kind, tau: p * anim.phys, p }
}
function valueColor(v: number, l: number, r: Run) {
  if (r.val[l] !== r.ref[l]) return r.flipped.includes(l >> 2) || flippedUpstream(l, r) ? COLORS.noise : COLORS.wrong
  return v ? COLORS.one : COLORS.zero
}
let upstreamCache: { run: Run; set: Set<number> } | null = null
/** links whose wrong value comes from a noise flip rather than a faulty cell */
function flippedUpstream(l: number, r: Run) {
  if (!upstreamCache || upstreamCache.run !== r) {
    const set = new Set<number>()
    if (r.flipped.length && !r.wrongCells.length) for (let i = 0; i < r.val.length; i++) if (r.val[i] >= 0 && r.val[i] !== r.ref[i]) set.add(i)
    upstreamCache = { run: r, set }
  }
  return upstreamCache.set.has(l)
}

function draw() {
  const dpr = window.devicePixelRatio || 1
  const W = cv.clientWidth, H = cv.clientHeight
  if (cv.width !== W * dpr || cv.height !== H * dpr) { cv.width = W * dpr; cv.height = H * dpr }
  ctx.setTransform(dpr, 0, 0, dpr, 0, 0)
  ctx.fillStyle = '#0b0e14'
  ctx.fillRect(0, 0, W, H)
  const t = now()
  if (!lay || !circuit) { requestAnimationFrame(draw); return }
  ctx.setTransform(dpr * scale, 0, 0, dpr * scale, dpr * ox, dpr * oy)
  const { w, h } = lay
  const testT = su && knobs.test ? su.time - maxReach() : 0
  const configured = (cell: number) => loaded || (t.kind === 'load' && su!.reach[cell] >= 0 && su!.reach[cell] + testT <= t.tau)
  // the clock: which level is ramping
  const T = run?.T ?? 1
  const D = circuit.depth
  let ramping = -1
  if (knobs.adiabatic && run && t.kind === 'run') ramping = Math.floor(t.tau / T)
  if (knobs.adiabatic && run && t.kind === 'flip') ramping = D - Math.floor(t.tau / T)
  // cells
  for (let y = 0; y < h; y++)
    for (let x = 0; x < w; x++) {
      const cell = y * w + x
      const faulty = !lay.sites.has(cell) && chip.faulty(x, y)
      const known = faulty && knobs.test && (loaded || t.kind !== 'load' || t.p > 0.12)
      ctx.fillStyle = known ? '#4a1820' : cellStage[cell] >= 0 && configured(cell) ? '#142033' : '#0f1520'
      ctx.fillRect(x * S + 1, y * S + 1, S - 2, S - 2)
      if (ramping >= 0 && cellStage[cell] === ramping) {
        ctx.fillStyle = 'rgba(179,156,255,0.28)'
        ctx.fillRect(x * S, y * S, S, S)
      }
      if (faulty) {
        ctx.strokeStyle = COLORS.wrong
        ctx.globalAlpha = known ? 0.8 : 0.45
        ctx.lineWidth = 1.2
        if (!known) ctx.setLineDash([2.5, 2.5])
        ctx.strokeRect(x * S + 2, y * S + 2, S - 4, S - 4)
        ctx.setLineDash([])
        if (known) {
          ctx.beginPath()
          ctx.moveTo(x * S + 6, y * S + 6); ctx.lineTo(x * S + S - 6, y * S + S - 6)
          ctx.moveTo(x * S + S - 6, y * S + 6); ctx.lineTo(x * S + 6, y * S + S - 6)
          ctx.stroke()
        }
        ctx.globalAlpha = 1
      }
      if (t.kind === 'load') {
        // the self-test flashes every cell; then the configuration front
        if (knobs.test && t.tau < testT) {
          ctx.fillStyle = `rgba(86,200,255,${0.12 + 0.1 * Math.sin(t.tau / testT * 20 + x * 0.7 + y * 0.3)})`
          ctx.fillRect(x * S, y * S, S, S)
        } else if (su!.reach[cell] >= 0) {
          const ahead = t.tau - testT - su!.reach[cell]
          const band = maxReach() * 0.06 + 1e-15
          if (ahead >= 0 && ahead < band) {
            ctx.fillStyle = `rgba(86,200,255,${0.55 * (1 - ahead / band)})`
            ctx.fillRect(x * S, y * S, S, S)
          }
        }
      }
    }
  // wires
  ctx.lineCap = 'round'
  for (const r of lay.routes)
    for (const l of r.links) {
      const p = paths[l]
      if (!p || !configured(l >> 2)) continue
      let color = COLORS.idle
      let width = 1.6
      let glow = 0
      if (run) {
        const v = run.val[l]
        const fresh = valueColor(v, l, run)
        const old = shown && shown.val[l] >= 0 ? valueColor(shown.val[l], l, shown) : COLORS.idle
        if (t.kind === 'run') {
          if (knobs.adiabatic) {
            const f = (t.tau - run.at[l]) / T
            color = mix(COLORS.idle, fresh, f)
            if (f > 0 && f < 1 && v) glow = 0.25
          } else {
            const arrived = t.tau >= run.at[l]
            color = arrived ? fresh : old
            const since = (t.tau - run.at[l]) / (anim!.phys + 1e-30)
            if (arrived && since < 0.12 && v !== (shown?.val[l] ?? 0)) glow = 1 - since / 0.12
          }
        } else if (t.kind === 'flip') {
          if (knobs.adiabatic) {
            const f = 1 - (t.tau - (D - run.stage[l]) * T) / T
            const caught = back?.caught.includes(l)
            color = caught && f < 1 ? COLORS.noise : mix(COLORS.idle, fresh, f)
            if (caught && f < 0.6 && f > 0) glow = 1
          } else {
            color = mix(fresh, COLORS.idle, t.p * 3)
            if (v && t.p < 0.5) glow = 1 - t.p * 2
          }
        } else color = charged ? fresh : COLORS.idle
        if (color === COLORS.one || color === COLORS.wrong || color === COLORS.noise) width = 2.2
      }
      if (glow > 0) {
        const [cx, cy] = center(l >> 2)
        const g = ctx.createRadialGradient(cx, cy, 0, cx, cy, S * 0.9)
        g.addColorStop(0, `rgba(255,158,100,${0.55 * glow})`)
        g.addColorStop(1, 'rgba(255,158,100,0)')
        ctx.fillStyle = g
        ctx.fillRect(cx - S, cy - S, 2 * S, 2 * S)
      }
      ctx.strokeStyle = color
      ctx.lineWidth = width
      ctx.stroke(p)
    }
  // gates and pads
  ctx.textAlign = 'center'
  ctx.textBaseline = 'middle'
  const font = getComputedStyle(document.body).fontFamily
  const GLYPH = { not: '¬', xor: '⊕', and: '∧', andn: '∧', or: '∨', orn: '∨', mux: '?' }
  circuit.gates.forEach((g, i) => {
    const cell = lay!.gateAt[i]
    if (!configured(cell)) return
    const [cx, cy] = center(cell)
    const out = run && (t.kind === 'idle' ? charged : t.kind === 'run' ? (knobs.adiabatic ? t.tau >= g.level * T : t.tau >= gateTime(i)) : t.kind === 'flip' && knobs.adiabatic && t.tau < (D - g.level + 1) * T)
    const bad = chip.faulty(cell % lay!.w, Math.floor(cell / lay!.w))
    ctx.fillStyle = bad ? '#4a1820' : '#1d2b40'
    ctx.strokeStyle = bad ? COLORS.wrong : out ? '#7f93ad' : '#3a4d68'
    ctx.lineWidth = 1.2
    ctx.beginPath()
    ctx.roundRect(cx - BOX, cy - BOX, 2 * BOX, 2 * BOX, 2.5)
    ctx.fill(); ctx.stroke()
    ctx.fillStyle = '#e6edf5'
    ctx.font = `${g.kind === 'mux' ? 9 : 10}px ${font}`
    ctx.fillText(GLYPH[g.kind], cx, cy + 0.5)
    if (g.kind === 'andn' || g.kind === 'orn') {
      ctx.beginPath(); ctx.arc(cx - BOX + 1.2, cy - BOX + 1.2, 1.6, 0, 7); ctx.fillStyle = '#e6edf5'; ctx.fill()
    }
  })
  const pad = (cell: number, v: number | null, label: string, left: boolean, wrong: boolean) => {
    const [cx, cy] = center(cell)
    ctx.beginPath()
    ctx.arc(cx, cy, BOX + 1.5, 0, 7)
    ctx.fillStyle = v === null ? '#142033' : v ? '#5a4a1c' : '#1a2c44'
    ctx.fill()
    ctx.strokeStyle = wrong ? COLORS.wrong : v === null ? '#3a4d68' : v ? COLORS.one : COLORS.zero
    ctx.lineWidth = 1.4
    ctx.stroke()
    ctx.fillStyle = '#e6edf5'
    ctx.font = `9px ${font}`
    if (v !== null) ctx.fillText(String(v), cx, cy + 0.5)
    ctx.fillStyle = '#8595ad'
    ctx.textAlign = left ? 'right' : 'left'
    ctx.fillText(label, cx + (left ? -S * 0.6 : S * 0.6), cy)
    ctx.textAlign = 'center'
  }
  circuit.inputs.forEach((p, i) => pad(lay!.inAt[i], bits[i], p.label, true, false))
  circuit.outputs.forEach((p, i) => {
    const ready = run && (t.kind === 'idle' ? charged : t.kind === 'run' && t.p > 0.98)
    pad(lay!.outAt[i], ready ? run!.outBits[i] : null, p.label + (p.net <= 1 ? ` = ${p.net}` : ''), false, !!ready && run!.outBits[i] !== run!.outRef[i])
  })
  // a frame around the chip's window
  ctx.strokeStyle = '#1b2436'
  ctx.lineWidth = 1
  ctx.strokeRect(0, 0, w * S, h * S)
  if (t.kind !== 'idle') hudTime(t)
  requestAnimationFrame(draw)
}
let reachMax = -1
function maxReach() {
  if (reachMax < 0 && su) for (const r of su.reach) reachMax = Math.max(reachMax, r)
  return reachMax
}
function gateTime(i: number) {
  const cell = lay!.gateAt[i]
  const r = lay!.routes.find((q) => q.src === cell)
  return r && r.links.length ? run!.at[r.links[0]] : 0
}

// ---- the readouts ----
function hud() {
  if (!circuit) { $('title').textContent = ''; return }
  const inV = withBits(circuit.input, circuit.regs, bits)
  const want = answer(circuit, bits)
  const got = run && !anim ? outputValue(circuit, run.outBits) : null
  const gone = got && !charged ? ' <span class="arrow">(flipped: the wires hold only the input again)</span>' : ''
  $('title').innerHTML = `${esc(circuit.src)} <span class="arrow">on</span> ${esc(show(inV))} <span class="arrow">→</span> ${got ? esc(show(got)) : '…'}${gone}`
  const v = $('verdict')
  if (got) {
    const ok = !!want && eq(want, got)
    v.className = ok ? 'ok' : 'bad'
    const why = run!.wrongCells.length ? ` (a faulty cell the chip didn't know of: turn the self-test on)` : run!.flipped.length ? (back?.caught.length ? ' (a noise flip, caught on the way back: run again)' : knobs.adiabatic ? ' (a noise flip: flip back to catch it)' : ' (a noise flip: a static clock has no way back to catch it)') : ''
    v.textContent = ok ? '✓ the two-way interpreter gives the same' : `✗ the two-way interpreter gives ${show(want)}${why}`
  } else { v.className = ''; v.textContent = '' }
  if (!lay || !su) { $('circ').textContent = ''; return }
  const kinds: Record<string, number> = {}
  for (const g of circuit.gates) kinds[g.kind] = (kinds[g.kind] ?? 0) + 1
  $('circ').innerHTML = `circuit: <b>${circuit.gates.length}</b> gates in <b>${circuit.depth}</b> levels · <b>${lay.wireCells}</b> wire cells · window ${lay.w}×${lay.h}`
  const faultyHere = countFaulty()
  $('chipline').innerHTML = `chip at <b>${knobs.vdd.toFixed(2)} V</b>, <b>${(knobs.c * 1e15).toPrecision(2)} fF</b>: margin ${(chip.margin * 1e3).toFixed(0)} mV against offsets of σ ${(chip.sigma * 1e3).toFixed(0)} mV → <b>${faultyHere}</b> faulty cells here (${fmtP(chip.faultShare * 100)}%)${knobs.test ? ', routed around' : ', <i>not tested</i>'}; noise flips a cell's output ${chip.transient < 1e-30 ? 'practically never' : `${fmtP(chip.transient)} of the time`}`
  if (run) {
    const heat = run.heat.switching + run.heat.leak
    const extra = back ? ` · flipped back: <i>${fmtE(back.heat)}</i> heat, <u>${fmtE(back.returned)}</u> returned${back.caught.length ? `, <i>caught ${back.caught.length} mismatched node${back.caught.length > 1 ? 's' : ''}: rerun</i>` : ', nothing caught'}` : drained ? ` · drained: <i>${fmtE(drained)}</i> more heat` : ''
    const time = knobs.adiabatic ? `${circuit.depth + 1} ramps × ${fmtT(run.T)} = <b>${fmtT(run.time)}</b>` : `settles in <b>${fmtT(run.settle)}</b> (clock ${fmtT(run.time)} with margin for supply noise)`
    $('runline').innerHTML = `run: ${time} · heat <i>${fmtE(heat)}</i> (switching ${fmtE(run.heat.switching)}, leakage ${fmtE(run.heat.leak)})${knobs.adiabatic ? ` · held on the wires <u>${fmtE(run.charged)}</u>` : ''}${extra}`
    const perRun = heat + (back?.heat ?? 0) + drained
    $('setupline').innerHTML = `set-up: ${knobs.test ? 'self-test and ' : ''}configuration wave ${fmtT(su.time)}, ${fmtE(su.energy)} (${su.bits.toLocaleString('en')} bits): ${Math.round(su.energy / Math.max(perRun, 1e-30)).toLocaleString('en')} runs' worth`
  } else {
    $('runline').textContent = ''
    $('setupline').innerHTML = `set-up: ${knobs.test ? 'self-test and ' : ''}configuration wave ${fmtT(su.time)}, ${fmtE(su.energy)} (${su.bits.toLocaleString('en')} bits)`
  }
}
function hudTime(t: { kind: string; tau: number }) {
  const el = $('story')
  const base = el.dataset.base ?? ''
  el.textContent = `${base} · t = ${fmtT(t.tau)}`
}
function countFaulty() {
  let n = 0
  for (let y = 0; y < lay!.h; y++) for (let x = 0; x < lay!.w; x++) if (!lay!.sites.has(y * lay!.w + x) && chip.faulty(x, y)) n++
  return n
}
function story() {
  const el = $('story')
  let s = ''
  if (anim?.kind === 'load') s = knobs.test ? 'set-up: every cell tests itself, then the configuration spreads from the left edge around the faulty ones' : 'set-up: no self-test, so the configuration lands on faulty cells too'
  else if (anim?.kind === 'run') s = knobs.adiabatic ? 'run: the power clock ramps one level at a time; each level charges slowly from the one before' : 'run: each cell switches when its inputs arrive; the wave ripples left to right'
  else if (anim?.kind === 'flip') s = knobs.adiabatic ? 'flip: the levels ramp down last first, and their charge goes back to the supply; each node is checked against its driver' : 'flip with a static clock: nothing to undo it with, so every node at 1 drains as heat'
  else s = loaded ? 'click an input pad to change a bit · drag to pan, wheel to zoom · hover a cell for its numbers' : ''
  el.dataset.base = s
  el.textContent = s
  hud()
}
const esc = (s: string) => s.replace(/[&<>]/g, (c) => ({ '&': '&amp;', '<': '&lt;', '>': '&gt;' })[c]!)

// ---- controls ----
const exSel = $<HTMLSelectElement>('ex')
for (const e of EXAMPLES) exSel.add(new Option(e.name, e.name))
function pick(name: string) {
  const e = EXAMPLES.find((x) => x.name === name)!
  $<HTMLInputElement>('prog').value = e.prog
  $<HTMLInputElement>('inp').value = e.input
  exSel.title = e.tip
  reachMax = -1
  compileNow()
}
exSel.onchange = () => pick(exSel.value)
for (const id of ['prog', 'inp']) $(id).addEventListener('keydown', (e) => { if ((e as KeyboardEvent).key === 'Enter') { reachMax = -1; compileNow() } })
$('load').onclick = () => { reachMax = -1; build(false) }
$('run').onclick = () => doRun()
$('flip').onclick = () => doFlip()
$('zap').onclick = () => doZap()
const vdd = $<HTMLInputElement>('vdd'), cap = $<HTMLInputElement>('cap'), ramp = $<HTMLInputElement>('ramp')
function labels() {
  $('vddv').textContent = `${(+vdd.value).toFixed(2)} V`
  $('capv').textContent = `${(10 ** +cap.value).toPrecision(2)} fF`
  $('rampv').textContent = fmtT(10 ** +ramp.value)
  $('mode').textContent = knobs.adiabatic ? 'adiabatic' : 'static'
  $('mode').classList.toggle('on', knobs.adiabatic)
  $('test').textContent = knobs.test ? 'self-test ✓' : 'self-test ✗'
  $('test').classList.toggle('on', knobs.test)
  ramp.disabled = !knobs.adiabatic
}
vdd.oninput = cap.oninput = ramp.oninput = labels
vdd.onchange = () => { knobs.vdd = +vdd.value; reachMax = -1; build(false) }
cap.onchange = () => { knobs.c = 10 ** +cap.value * 1e-15; reachMax = -1; build(false) }
ramp.onchange = () => { knobs.ramp = 10 ** +ramp.value; chip = new Chip({ ...knobs }); if (loaded) { charged = false; run = null; doRun() } }
$('mode').onclick = () => { knobs.adiabatic = !knobs.adiabatic; labels(); chip = new Chip({ ...knobs }); if (loaded) { charged = false; run = null; doRun() } }
$('test').onclick = () => { knobs.test = !knobs.test; labels(); reachMax = -1; build(false) }
$('chip').onclick = () => { knobs.seed++; reachMax = -1; build(false) }
window.addEventListener('keydown', (e) => {
  if ((e.target as HTMLElement).tagName === 'INPUT' && (e.target as HTMLInputElement).type !== 'range') return
  if (e.key === ' ') { e.preventDefault(); doRun() } else if (e.key === 'f' || e.key === 'F') doFlip()
  else if (e.key === 'z' || e.key === 'Z') doZap()
  else if (e.key === 'l' || e.key === 'L') { reachMax = -1; build(false) }
})

// pan, zoom, pads, hover
let drag: { x: number; y: number; ox: number; oy: number; moved: boolean } | null = null
const toWorld = (e: MouseEvent) => [(e.offsetX - ox) / scale, (e.offsetY - oy) / scale]
cv.addEventListener('mousedown', (e) => { drag = { x: e.clientX, y: e.clientY, ox, oy, moved: false }; cv.classList.add('drag') })
window.addEventListener('mouseup', (e) => {
  cv.classList.remove('drag')
  if (drag && !drag.moved && lay && circuit) {
    const [wx, wy] = toWorld(e)
    const cell = Math.floor(wy / S) * lay.w + Math.floor(wx / S)
    const i = lay.inAt.indexOf(cell)
    if (i >= 0 && wx >= 0) { bits[i] ^= 1; $<HTMLInputElement>('inp').value = show(withBits(circuit.input, circuit.regs, bits)); doRun() }
  }
  drag = null
})
cv.addEventListener('mousemove', (e) => {
  if (drag) {
    if (Math.abs(e.clientX - drag.x) + Math.abs(e.clientY - drag.y) > 3) drag.moved = true
    ox = drag.ox + e.clientX - drag.x
    oy = drag.oy + e.clientY - drag.y
  }
  tip(e)
})
cv.addEventListener('mouseleave', () => ($('tip').style.display = 'none'))
cv.addEventListener('wheel', (e) => {
  e.preventDefault()
  const f = Math.exp(-e.deltaY * 0.0015)
  ox = e.offsetX - (e.offsetX - ox) * f
  oy = e.offsetY - (e.offsetY - oy) * f
  scale *= f
}, { passive: false })
window.addEventListener('resize', fit)
function tip(e: MouseEvent) {
  const el = $('tip')
  if (!lay || drag?.moved) { el.style.display = 'none'; return }
  const [wx, wy] = toWorld(e)
  const x = Math.floor(wx / S), y = Math.floor(wy / S)
  if (x < 0 || y < 0 || x >= lay.w || y >= lay.h) { el.style.display = 'none'; return }
  const cell = y * lay.w + x
  const site = lay.sites.get(cell)
  const nets = [0, 1, 2, 3].map((d) => cell * 4 + d).filter((l) => lay!.linkNet[l] >= 0)
  const what = site?.k === 'gate' ? `gate ${circuit!.gates[site.i].kind}, level ${circuit!.gates[site.i].level}` : site?.k === 'in' ? `input pad ${circuit!.inputs[site.i].label}` : site?.k === 'out' ? `output pad ${circuit!.outputs[site.i].label}` : nets.length ? `wire cell, ${nets.length} link${nets.length > 1 ? 's' : ''} out` : 'unused'
  const off = chip.offset(x, y)
  const lines = [
    `cell (${x}, ${y}): ${what}`,
    `worst threshold offset ${(off * 1e3).toFixed(1)} mV, margin ${(chip.margin * 1e3).toFixed(0)} mV${!site && chip.faulty(x, y) ? `: FAULTY, stuck at ${chip.stuck(x, y)}` : ''}`,
    `delay ${fmtT(chip.delay(x, y, site?.k === 'gate'))}`
  ]
  if (run) for (const l of nets) lines.push(`  out ${'NESW'[l & 3]}: ${run.val[l]}${run.val[l] !== run.ref[l] ? ` (should be ${run.ref[l]})` : ''}, ${knobs.adiabatic ? `level ${run.stage[l]}` : `settles at ${fmtT(run.at[l])}`}`)
  el.textContent = lines.join('\n')
  el.style.display = 'block'
  el.style.left = `${Math.min(e.clientX + 14, window.innerWidth - el.offsetWidth - 8)}px`
  el.style.top = `${e.clientY + 14}px`
}

labels()
pick(EXAMPLES[6].name)
exSel.value = EXAMPLES[6].name
requestAnimationFrame(draw)
