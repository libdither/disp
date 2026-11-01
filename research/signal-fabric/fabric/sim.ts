// The array's devices and a run on it. Each cell's few gates came out with threshold offsets (mismatch,
// shrinking as the cell's capacitance grows); a cell whose worst offset exceeds its noise margin at this
// supply is faulty, stuck at one value. Signals are full-swing levels each cell restores; a run follows
// every routed link's value and when it settles, and counts the energy, from the device model in
// ../model.mjs. Static clocking switches each node as its inputs arrive; adiabatic clocking ramps a
// level's nodes up slowly in its own phase, level after level, and flipping runs the phases back down,
// so the charge returns to the supply.
import * as m from '../model.mjs'
import { gateOut, type Circuit } from './circuit.ts'
import { cellAcross, type Layout } from './layout.ts'

export interface Knobs {
  vdd: number // supply (V)
  c: number // a gate's switched capacitance (F): bigger is slower and costlier, but quieter and better matched
  adiabatic: boolean
  ramp: number // adiabatic power-clock ramp (s)
  test: boolean // self-test at set-up and route around faulty cells
  seed: number // which chip: its cells' offsets
}

const P = m.P
/** A number in [0, 1) fixed by the chip, the cell and k. */
function hash(seed: number, x: number, y: number, k: number) {
  let h = Math.imul(seed ^ 0x9e3779b9, 0x85ebca6b) ^ Math.imul(x + 0x632be5ab, 0xc2b2ae35) ^ Math.imul(y + 0x2f1d4c3b, 0x27d4eb2f) ^ Math.imul(k + 1, 0x165667b1)
  h ^= h >>> 15; h = Math.imul(h, 0x2c1b3c6d); h ^= h >>> 12; h = Math.imul(h, 0x297a2d39); h ^= h >>> 15
  return (h >>> 0) / 4294967296
}
const gaussAt = (seed: number, x: number, y: number, k: number) =>
  Math.sqrt(-2 * Math.log(hash(seed, x, y, 2 * k) + 1e-12)) * Math.cos(2 * Math.PI * hash(seed, x, y, 2 * k + 1))

export class Chip {
  readonly p: typeof P
  readonly margin: number
  readonly sigma: number
  constructor(readonly k: Knobs) {
    this.p = { ...P, vdd: k.vdd, c: k.c, ramp: k.ramp }
    this.margin = m.margin(k.vdd, this.p)
    this.sigma = m.mismatch(k.c, this.p)
  }
  private offsets = new Map<number, number>()
  private delays = new Map<number, number>()
  /** The worst threshold offset among the cell's gates (V). */
  offset(x: number, y: number) {
    const key = y * 65536 + x
    let o = this.offsets.get(key)
    if (o === undefined) {
      let worst = 0
      for (let g = 0; g < P.cellGates; g++) {
        const z = gaussAt(this.k.seed, x, y, g)
        if (Math.abs(z) > Math.abs(worst)) worst = z
      }
      this.offsets.set(key, (o = worst * this.sigma))
    }
    return o as number
  }
  faulty(x: number, y: number) { return Math.abs(this.offset(x, y)) > this.margin }
  /** A faulty cell's output: an input threshold pushed past the swing never sees a 1, or always does. */
  stuck(x: number, y: number) { return this.offset(x, y) > 0 ? 0 : 1 }
  /** A cell's delay: its slowest gate, whose threshold is its offset higher. */
  delay(x: number, y: number, gate: boolean) {
    const key = (y * 65536 + x) * 2 + (gate ? 1 : 0)
    let d = this.delays.get(key)
    if (d === undefined) {
      const p = { ...this.p, vt: P.vt + Math.abs(this.offset(x, y)) }
      const fresh: number = gate ? P.cellDelay * m.gateDelay(this.k.vdd, this.k.c, p) : m.gateDelay(this.k.vdd, P.hopC + this.k.c / 2, p)
      this.delays.set(key, (d = fresh))
    }
    return d
  }
  /** Capacitance switched when a cell's output link changes. */
  cap(gate: boolean) { return gate ? P.cellGates * this.k.c : P.hopC + this.k.c / 2 }
  /** Chance a cell's output is flipped by thermal noise in one run, and the share of cells built faulty. */
  get transient() { return m.gateErr(this.k.vdd, this.k.c, this.p) }
  get faultShare() { return 1 - (1 - m.gateFault(this.k.vdd, this.k.c, this.p)) ** P.cellGates }
}

export interface Run {
  val: Int8Array // per link: the value on it (-1 unused)
  ref: Int8Array // the value a perfect fabric would carry
  at: Float64Array // static: when it settles; adiabatic: when its level's ramp starts (s)
  stage: Int32Array // per link: the level that drives it
  outBits: number[]
  outRef: number[]
  flipped: number[] // cells whose output noise flipped this run
  wrongCells: number[] // faulty cells a wrong value came out of
  time: number // s until every output is good: static, the settling time with a margin for supply noise
  settle: number
  T: number // the ramp each level takes (adiabatic)
  heat: { switching: number; leak: number }
  charged: number // energy held on the nodes at 1 (J), given back by flipping adiabatically
}

/** One run on input `bits`: noise strikes each cell with its chance (`rand`), and `zap` flips one cell's output. */
export function simulate(c: Circuit, lay: Layout, chip: Chip, bits: number[], prev: Run | null, zap: number | null, rand: () => number): Run {
  const { w, routes } = lay
  const n = w * lay.h * 4
  const val = new Int8Array(n).fill(-1)
  const ref = new Int8Array(n).fill(-1)
  const at = new Float64Array(n)
  const stage = new Int32Array(n)
  const flipped: number[] = []
  const wrongCells = new Set<number>()
  const k = chip.k
  const xy = (cell: number) => [cell % w, Math.floor(cell / w)] as const
  const isFaulty = (cell: number) => chip.faulty(...xy(cell))
  const pErr = chip.transient
  const noise = (cell: number) => cell === zap || (pErr > 1e-12 && rand() < pErr)
  const routeOf = new Map<number, number>()
  routes.forEach((r, i) => routeOf.set(r.net, i))
  const level = new Map<number, number>()
  c.inputs.forEach((p) => level.set(p.net, 0))
  c.gates.forEach((g) => level.set(g.net, g.level))
  /** a cell's output, from what came in: stuck if faulty, flipped if noise strikes */
  const isPad = (cell: number) => lay.sites.get(cell)?.k === 'in' || lay.sites.get(cell)?.k === 'out'
  const pass = (cell: number, v: number) => {
    let o = v
    if (!isPad(cell) && isFaulty(cell)) { o = chip.stuck(...xy(cell)); if (o !== v) wrongCells.add(cell) }
    if (noise(cell)) { o ^= 1; flipped.push(cell) }
    return o
  }
  const delay = (cell: number) => chip.delay(...xy(cell), lay.sites.get(cell)?.k === 'gate')
  const driveTime = new Map<number, number>()
  const driveVal = new Map<number, [number, number]>()
  c.inputs.forEach((p, i) => {
    const cell = lay.inAt[i]
    driveVal.set(p.net, [pass(cell, bits[i]), bits[i]])
    driveTime.set(p.net, delay(cell))
  })
  const spread = (net: number) => {
    const ri = routeOf.get(net)
    if (ri === undefined) return
    const r = routes[ri]
    const [v0, r0] = driveVal.get(net)!
    const inLink = new Map<number, number>()
    for (const l of r.links) {
      const u = l >> 2
      if (u === r.src) { val[l] = v0; ref[l] = r0; at[l] = driveTime.get(net)! }
      else {
        const li = inLink.get(u)!
        val[l] = pass(u, val[li])
        ref[l] = ref[li]
        at[l] = at[li] + delay(u)
      }
      stage[l] = level.get(net) ?? 0
      inLink.set(cellAcross(l, w), l)
    }
  }
  c.inputs.forEach((p) => spread(p.net))
  const into = (net: number, cell: number) => routes[routeOf.get(net)!].into.get(cell)!
  const read = (net: number, cell: number, arr: Int8Array) => (net <= 1 ? net : arr[into(net, cell)])
  for (let gi = 0; gi < c.gates.length; gi++) {
    const g = c.gates[gi]
    const cell = lay.gateAt[gi]
    const got = gateOut(g.kind, g.ins.map((x) => read(x, cell, val)))
    const want = gateOut(g.kind, g.ins.map((x) => read(x, cell, ref)))
    driveVal.set(g.net, [pass(cell, got), want])
    driveTime.set(g.net, Math.max(0, ...g.ins.map((x) => (x <= 1 ? 0 : at[into(x, cell)]))) + delay(cell))
    spread(g.net)
  }
  const outBits = c.outputs.map((p, i) => read(p.net, lay.outAt[i], val))
  const outRef = c.outputs.map((p, i) => read(p.net, lay.outAt[i], ref))

  // time: static, until the last output settles; adiabatic, a ramp per level, each slow enough for its wires
  const settle = Math.max(0, ...c.outputs.map((p, i) => (p.net <= 1 ? 0 : at[into(p.net, lay.outAt[i])])))
  let longest = 0
  for (const r of routes) {
    const t0 = driveTime.get(r.net)!
    for (const l of r.links) longest = Math.max(longest, at[l] - t0)
  }
  const guard = m.guard(k.vdd, chip.p)
  const T = Math.max(k.ramp, 2 * (longest + P.cellDelay * m.gateDelay(k.vdd, k.c, chip.p)) * guard)
  if (k.adiabatic) for (let l = 0; l < n; l++) if (val[l] >= 0) at[l] = stage[l] * T
  const time = k.adiabatic ? (c.depth + 1) * T : settle * guard

  // energy: static, half C·V² for each node that changes; adiabatic, the ramp's resistive loss instead
  let switching = 0
  let charged = 0
  const v2 = k.vdd * k.vdd
  for (let l = 0; l < n; l++) {
    if (val[l] < 0) continue
    const u = l >> 2
    const isGate = lay.sites.get(u)?.k === 'gate'
    const C = chip.cap(isGate)
    if (val[l]) charged += 0.5 * C * v2
    if (k.adiabatic) { if (val[l]) switching += C * v2 * Math.max((2 * delay(u)) / T, P.clockLoss) }
    else if (val[l] !== (prev?.val[l] ?? 0)) switching += 0.5 * C * v2
  }
  const cellsOn = c.gates.length + lay.wireCells + c.inputs.length + c.outputs.length
  const leak = cellsOn * P.cellGates * m.leak(k.vdd, chip.p) * k.vdd * time
  return { val, ref, at, stage, outBits, outRef, flipped, wrongCells: [...wrongCells], time, settle, T, heat: { switching, leak }, charged }
}

/** Flipping an adiabatic run: the levels ramp back down, last first, so each node's charge returns. As a
 * node comes down it is compared with what its driver gives from the values still held: a node noise
 * flipped no longer matches and is caught (it dissipates its charge instead). A stuck cell gives the same
 * wrong value both ways and is not caught: the self-test is for those. */
export function retract(c: Circuit, lay: Layout, chip: Chip, run: Run, bits: number[]) {
  const { w } = lay
  const v2 = chip.k.vdd * chip.k.vdd
  const xy = (cell: number) => [cell % w, Math.floor(cell / w)] as const
  const isPad = (cell: number) => lay.sites.get(cell)?.k === 'in' || lay.sites.get(cell)?.k === 'out'
  const out = (cell: number, v: number) => (!isPad(cell) && chip.faulty(...xy(cell)) ? chip.stuck(...xy(cell)) : v)
  const routeOf = new Map<number, number>()
  lay.routes.forEach((r, i) => routeOf.set(r.net, i))
  const held = (net: number, cell: number) => (net <= 1 ? net : run.val[lay.routes[routeOf.get(net)!].into.get(cell)!])
  const drives = new Map<number, number>() // driver cell → what it gives now
  c.inputs.forEach((_, i) => drives.set(lay.inAt[i], bits[i]))
  c.gates.forEach((g, i) => drives.set(lay.gateAt[i], out(lay.gateAt[i], gateOut(g.kind, g.ins.map((x) => held(x, lay.gateAt[i]))))))
  let heat = 0, returned = 0
  const caught: number[] = []
  for (const r of lay.routes) {
    const inLink = new Map<number, number>()
    for (const l of r.links) {
      const u = l >> 2
      const want = u === r.src ? drives.get(u)! : out(u, run.val[inLink.get(u)!])
      inLink.set(cellAcross(l, w), l)
      const gate = lay.sites.get(u)?.k === 'gate'
      const C = chip.cap(gate)
      if (run.val[l] !== want) { caught.push(l); heat += 0.5 * C * v2 }
      else if (run.val[l]) {
        heat += C * v2 * Math.max((2 * chip.delay(...xy(u), gate)) / run.T, P.clockLoss)
        returned += 0.5 * C * v2
      }
    }
  }
  return { heat, returned, caught }
}

/** Set-up: every cell in the window tests itself, then the configuration spreads cell to cell from the
 * input edge, around the cells found faulty. Returns per cell the time the wave reaches it (or -1). */
export function setup(lay: Layout, chip: Chip) {
  const { w, h } = lay
  const k = chip.k
  const hopT = m.gateDelay(k.vdd, P.hopC + k.c / 2, chip.p)
  const reach = new Float64Array(w * h).fill(-1)
  const q: number[] = []
  for (let y = 0; y < h; y++) { reach[y * w] = 0; q.push(y * w) } // the input edge loads
  for (let i = 0; i < q.length; i++) {
    const cell = q[i]
    const x = cell % w, y = Math.floor(cell / w)
    for (const [dx, dy] of [[1, 0], [0, 1], [0, -1], [-1, 0]]) {
      const nx = x + dx, ny = y + dy
      if (nx < 0 || ny < 0 || nx >= w || ny >= h) continue
      const nc = ny * w + nx
      if (reach[nc] >= 0 || (k.test && chip.faulty(nx, ny))) continue
      reach[nc] = reach[cell] + 1
      q.push(nc)
    }
  }
  let far = 0
  for (const r of reach) far = Math.max(far, r)
  const perHop = P.cfgBits * hopT
  for (let i = 0; i < reach.length; i++) if (reach[i] >= 0) reach[i] *= perHop
  const area = w * h
  const configured = new Set<number>(lay.sites.keys())
  for (const r of lay.routes) for (const l of r.links) configured.add(l >> 2)
  const v2 = k.vdd * k.vdd
  const testE = k.test ? area * 64 * P.cellGates * P.activity * k.c * v2 : 0
  const testT = k.test ? 64 * P.cellDelay * m.gateDelay(k.vdd, k.c, chip.p) : 0
  const cfgE = configured.size * P.cfgBits * (P.cfgWriteC * v2 + (Math.sqrt(area) * P.hopC * v2) / 4)
  return { reach, time: testT + far * perHop, energy: testE + cfgE, testE, cfgE, configured: configured.size, bits: configured.size * P.cfgBits }
}
