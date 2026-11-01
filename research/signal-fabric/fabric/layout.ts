// A circuit laid out on a grid of identical cells, each with a one-wire link to and from each of its four
// neighbours (as in the Cell Matrix). A gate takes a cell; a wire passes through cells, each hop a
// restoring buffer, and several wires can share a cell on different links. The array has tested itself:
// cells it found faulty are left out, and placement and routing go around them.
import type { Circuit } from './circuit.ts'

export const DX = [0, 1, 0, -1] // north, east, south, west
export const DY = [-1, 0, 1, 0]
export const opp = (d: number) => (d + 2) & 3

/** A site holding a gate or a pad. */
export type Site = { k: 'gate'; i: number } | { k: 'in'; i: number } | { k: 'out'; i: number }
/** One net routed: its driver, its links in an order where each link's cell is the driver or was reached
 * by an earlier link, and for each reader the link that brings the net in. */
export interface Route { net: number; src: number; links: number[]; into: Map<number, number> }
export interface Layout {
  w: number
  h: number
  sites: Map<number, Site> // cell → what it holds
  gateAt: number[]
  inAt: number[]
  outAt: number[]
  routes: Route[]
  /** link → the route using it; a link is cell·4 + direction, the wire out of the cell that way. */
  linkNet: Int32Array
  /** cells that carry at least one wire without holding a gate or pad */
  wireCells: number
  avoided: number // faulty cells the layout went around (inside its box)
}

/** Lays out c on a w×h window of the array; `bad(x, y)` is whether the self-test found the cell faulty. */
export function layout(c: Circuit, bad: (x: number, y: number) => boolean): Layout {
  for (let grow = 0; grow < 4; grow++) {
    const l = tryLayout(c, bad, 3 + grow, 2 + grow)
    if (l) return l
  }
  throw new Error('could not route this circuit around the faulty cells')
}

function tryLayout(c: Circuit, bad: (x: number, y: number) => boolean, px: number, py: number): Layout | null {
  const levels: number[][] = Array.from({ length: c.depth + 1 }, () => [])
  c.gates.forEach((g, i) => levels[g.level].push(i))
  const rows = Math.max(c.inputs.length, c.outputs.length, ...levels.map((l) => l.length))
  const h = rows * py + 3
  const w = (c.depth + 1) * px + 2
  const sites = new Map<number, Site>()
  const at = (x: number, y: number) => y * w + x
  const yOfNet = new Map<number, number>()
  const free = (x: number, y: number) => x > 0 && x < w - 1 && y >= 1 && y < h - 1 && !sites.has(at(x, y)) && !bad(x, y)
  /** spreads wanted rows apart by at least `py`, keeping their order, within the window */
  const spread = (want: number[]) => {
    const order = want.map((_, i) => i).sort((a, b) => want[a] - want[b])
    const ys = new Array<number>(want.length)
    let prev = -Infinity
    for (const i of order) prev = ys[i] = Math.max(Math.round(want[i]), prev + py, 1)
    const over = prev - (h - 2)
    if (over > 0) for (const i of order) ys[i] -= over
    for (let k = order.length - 1, next = Infinity; k >= 0; k--) {
      const i = order[k]
      ys[i] = Math.min(ys[i], next - py)
      next = ys[i]
    }
    return ys.map((y) => Math.max(1, y))
  }
  const top = Math.floor((h - c.inputs.length * py) / 2) + 1
  const inAt = c.inputs.map((p, i) => {
    const y = top + i * py
    sites.set(at(0, y), { k: 'in', i })
    yOfNet.set(p.net, y)
    return at(0, y)
  })
  const gateAt = new Array<number>(c.gates.length)
  for (let lv = 1; lv <= c.depth; lv++) {
    const gs = levels[lv]
    // each gate wants the mean row of what it reads
    const want = gs.map((gi) => {
      const ys = c.gates[gi].ins.map((n) => yOfNet.get(n) ?? h / 2)
      return ys.reduce((a, b) => a + b, 0) / ys.length
    })
    const ys = spread(want)
    gs.forEach((gi, k) => {
      const x0 = lv * px
      // the nearest good free cell to the wanted spot
      let best = -1
      for (let r = 0; r < 6 && best < 0; r++)
        for (let dy = -r; dy <= r && best < 0; dy++)
          for (let dx = -r; dx <= r && best < 0; dx++)
            if (Math.abs(dx) + Math.abs(dy) === r && free(x0 + dx, ys[k] + dy)) best = at(x0 + dx, ys[k] + dy)
      if (best < 0) best = at(x0, ys[k])
      sites.set(best, { k: 'gate', i: gi })
      gateAt[gi] = best
      yOfNet.set(c.gates[gi].net, Math.floor(best / w))
    })
  }
  const outWant = c.outputs.map((p) => yOfNet.get(p.net) ?? h / 2)
  const outTop = Math.floor((h - c.outputs.length * py) / 2) + 1
  const outAt = c.outputs.map((_, i) => {
    const y = c.outputs.length * py <= h - 2 ? outTop + i * py : spread(outWant)[i]
    sites.set(at(w - 1, y), { k: 'out', i })
    return at(w - 1, y)
  })

  // what each net must reach
  const readers = new Map<number, number[]>()
  const reads = (net: number, cell: number) => { if (net > 1) readers.set(net, [...(readers.get(net) ?? []), cell]) }
  c.gates.forEach((g, i) => g.ins.forEach((n) => reads(n, gateAt[i])))
  c.outputs.forEach((p, i) => reads(p.net, outAt[i]))
  const driver = new Map<number, number>()
  c.inputs.forEach((p, i) => driver.set(p.net, inAt[i]))
  c.gates.forEach((g, i) => driver.set(g.net, gateAt[i]))
  const nets = [...readers.keys()].filter((n) => driver.has(n))

  const routes = route(w, h, sites, bad, nets.map((net) => ({ net, src: driver.get(net)!, sinks: readers.get(net)! })))
  if (!routes) return null
  const linkNet = new Int32Array(w * h * 4).fill(-1)
  const used = new Set<number>()
  routes.forEach((r, k) => r.links.forEach((l) => { linkNet[l] = k; used.add(cellAcross(l, w)) }))
  let wireCells = 0
  for (const cell of used) if (!sites.has(cell)) wireCells++
  let avoided = 0
  for (let y = 0; y < h; y++) for (let x = 0; x < w; x++) if (bad(x, y)) avoided++
  return { w, h, sites, gateAt, inAt, outAt, routes, linkNet, wireCells, avoided }
}

/** The cell a link leads into. */
export const cellAcross = (l: number, w: number) => {
  const cell = l >> 2, d = l & 3
  return cell + DX[d] + DY[d] * w
}

// ---- routing: negotiated congestion (PathFinder, McMurchie and Ebeling): every net takes its cheapest
// tree; links wanted by more than one net get dearer, every round, until no link is shared.
interface Want { net: number; src: number; sinks: number[] }
function route(w: number, h: number, sites: Map<number, Site>, bad: (x: number, y: number) => boolean, nets: Want[]): Route[] | null {
  const n = w * h
  const hist = new Float64Array(n * 4)
  const occ = new Int32Array(n * 4)
  const blocked = new Uint8Array(n)
  for (let c = 0; c < n; c++) if (bad(c % w, Math.floor(c / w)) && !sites.has(c)) blocked[c] = 1
  const routes: Route[] = nets.map((t) => ({ net: t.net, src: t.src, links: [], into: new Map() }))
  const dist = new Float64Array(n)
  const via = new Int32Array(n)
  const seen = new Int32Array(n) // search stamp
  const mine = new Int32Array(n) // net stamp: the cell is on the tree being grown
  let search = 0, grown = 0
  const heap = new Heap()
  const order = nets.map((_, i) => i).sort((a, b) => nets[a].sinks.length - nets[b].sinks.length)
  const manhattan = (a: number, b: number) => Math.abs((a % w) - (b % w)) + Math.abs(Math.floor(a / w) - Math.floor(b / w))
  let pres = 0.5
  for (let round = 0; round < 80; round++) {
    for (const k of order) {
      const t = nets[k]
      for (const l of routes[k].links) occ[l]--
      const r: Route = { net: t.net, src: t.src, links: [], into: new Map() }
      mine[t.src] = ++grown
      const tree = [t.src]
      for (const sink of [...t.sinks].sort((a, b) => manhattan(a, t.src) - manhattan(b, t.src))) {
        search++
        heap.clear()
        for (const c of tree) { dist[c] = 0; via[c] = -1; seen[c] = search; heap.push(c, manhattan(c, sink)) }
        let found = false
        while (heap.size) {
          const [u, f] = heap.pop()
          if (f > dist[u] + manhattan(u, sink) + 1e-9) continue
          if (u === sink) { found = true; break }
          if (u !== t.src && sites.has(u)) continue // only the driver and plain wire cells pass a signal on
          const ux = u % w, uy = Math.floor(u / w)
          for (let d = 0; d < 4; d++) {
            const x = ux + DX[d], y = uy + DY[d]
            if (x < 0 || y < 0 || x >= w || y >= h) continue
            const v = y * w + x
            if (blocked[v] || mine[v] === grown || (sites.has(v) && v !== sink)) continue
            const l = u * 4 + d
            const nd = dist[u] + (1 + hist[l]) * (1 + pres * occ[l])
            if (seen[v] !== search || nd < dist[v] - 1e-9) {
              seen[v] = search
              dist[v] = nd
              via[v] = l
              heap.push(v, nd + manhattan(v, sink))
            }
          }
        }
        if (!found) return null
        const path: number[] = []
        for (let c = sink; mine[c] !== grown; c = via[c] >> 2) path.push(via[c])
        path.reverse()
        for (const l of path) {
          r.links.push(l)
          const v = cellAcross(l, w)
          if (v !== sink) { mine[v] = grown; tree.push(v) }
        }
        r.into.set(sink, path[path.length - 1])
      }
      for (const l of r.links) occ[l]++
      routes[k] = r
    }
    let over = 0
    for (let l = 0; l < n * 4; l++) if (occ[l] > 1) { over++; hist[l] += 0.6 }
    if (!over) return routes
    pres *= 1.6
  }
  return null
}

class Heap {
  private k: number[] = []
  private p: number[] = []
  get size() { return this.k.length }
  clear() { this.k.length = 0; this.p.length = 0 }
  push(key: number, pri: number) {
    const k = this.k, p = this.p
    let i = k.length
    k.push(key); p.push(pri)
    while (i > 0) {
      const j = (i - 1) >> 1
      if (p[j] <= pri) break
      k[i] = k[j]; p[i] = p[j]; i = j
    }
    k[i] = key; p[i] = pri
  }
  pop(): [number, number] {
    const k = this.k, p = this.p
    const top: [number, number] = [k[0], p[0]]
    const lk = k.pop()!, lp = p.pop()!
    if (k.length) {
      let i = 0
      for (;;) {
        let j = 2 * i + 1
        if (j >= k.length) break
        if (j + 1 < k.length && p[j + 1] < p[j]) j++
        if (p[j] >= lp) break
        k[i] = k[j]; p[i] = p[j]; i = j
      }
      k[i] = lk; p[i] = lp
    }
    return top
  }
}
