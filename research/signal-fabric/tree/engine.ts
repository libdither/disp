// The value is a tree laid out on a lattice: every node sits in a cell, at most one a cell, and every edge
// is a wire running between its two nodes' cells. The program is not laid out anywhere: it travels as
// signals. A signal carrying a program moves one cell a tick along a wire to the node it is for, and the
// program reduces there, by core.ts's rules: `map` sends one signal down each wire at once, so a word is
// worked on at all its nodes in parallel; `tri` reads the left child and picks; when a node is done a
// signal goes back up.
//
// Everything that changes the tree is a rule: a little tree whose loose ends are holes (parts passed along
// unread), and the same nodes and holes rewired, with nodes made or dropped. The five moves are the
// machine's own rules, and so are tri's: taking the stem off its left part and putting it back, and
// rotating. A piece of the program that reads and makes only a few nodes is learned as a rule of its own
// the first time it runs: its left side is the nodes it read, its right side what it left there. When the
// piece reaches a node again (in this run, or in a run on other bits) and the left side fits, the rule
// does in one tick what took the moves many; reading the left side costs a round trip to its farthest
// node. So the first run builds the rules a later run uses, from the bits up.
//
// Matter does not wait for the program: every tick each node may step to a free neighbouring cell where
// its wires are shorter (each wire pulls a child to sit a little under its parent). So after a rewrite
// hangs a subtree from a new node, the nodes drift back into shape on their own, and small values (a
// carry, a status) follow whatever reads them, while the word, pulled every way at once, stays put.
//
// The run is simulated event by event. How much it charges is a setting, `Physics`, one field for each
// shortcut a lattice could not take: HONEST charges for all of them. Learned rules ride with the program's
// signals, so a node knows only the rules that reached it by wire. A signal takes its wire's length plus
// its size in symbols, one signal at a time each way on a wire. A rewrite waits until its writes reach the
// farthest node it changes. Wires are paths of cells: a node that steps drags its end of each wire one
// cell, a rewrite splices new wires along the old ones, kinks straighten a cell at a time, and a node steps
// only where its wires' first cells say it should. CHEAP takes every shortcut (one book every node reads at
// once, free sizes and writes, wires drawn fresh from their ends); it gives the first version's numbers, and
// is kept to measure what each shortcut was worth. Some bookkeeping is still free: the record of what a
// piece read travels with its done signal and is charged there, but which cell holds which wire is the
// simulator's, and a node's matter-moving step runs one node at a time within a tick.
import { form, bodyOf, key, L as VL, S as VS, F as VF, type V } from '../../../website/src/lib/evaluators/twoway/core.ts'
import { nameOf, progLabel } from '../../../website/src/lib/evaluators/twoway/text.ts'

export type Kind = 'L' | 'S' | 'F' | 'R'
export type Layout = 'rows' | 'packed'
export type Cell = [number, number]
export interface Node { id: number; kind: Kind; x: number; y: number; kids: number[] }
/** A wire from a node's port down to a child; with wires of cells, the cells it runs through, parent first. */
export interface Link { id: number; from: number; to: number; path?: Cell[] }

/** What the machine may do without paying for it. */
export interface Physics {
  /** Where learned rules live: in one book that every node reads at once ('shared', which no lattice has);
   * riding with the program's signals, so a node knows only the rules that reached it by wire ('carried':
   * a wire carries a rule down once, the node at its end keeps it, and rules learned below come back up
   * with done signals); or in the cell where a rule was learned, for whatever runs in that cell later
   * ('cell'). */
  rules: 'shared' | 'carried' | 'cell'
  /** A signal takes its wire's length plus its size in symbols, and a wire carries one signal at a time each
   * way; else just its length, with any number at once. */
  sizes: boolean
  /** A rewrite waits until its writes reach the farthest node it changes; else one tick, however far. */
  writes: boolean
  /** 'cells': a wire is a path of cells that changes a cell at a time, and a cell carries at most LANES
   * wires passing through it; 'drawn': a wire is a shortest path drawn fresh from its two ends whenever
   * either moves, through any number of others. */
  wires: 'drawn' | 'cells'
}
export const CHEAP: Physics = { rules: 'shared', sizes: false, writes: false, wires: 'drawn' }
export const HONEST: Physics = { rules: 'carried', sizes: true, writes: true, wires: 'cells' }
/** The most wires a node's step or a climb may leave passing through one cell. */
export const LANES = 4
export const DX = [0, 1, 0, -1]
export const DY = [-1, 0, 1, 0]
/** Rows a child likes to sit under its parent. */
const REST = 2
/** How much more a wire minds being off its row than being off to the side, so the tree keeps its rows. */
const STIFF = 3
/** The most nodes a piece may read and make and still be learned as a rule. */
export const RULE_MAX = 16

/** One side of a rule: a little tree whose holes are parts the rule passes along unread. A node that
 * stays the same node from one side to the other has the same `i` on both; one without is made. */
export type Pat = { hole: number } | { kind: 'L' | 'S' | 'F'; kids: Pat[]; i?: number }
export interface Rule {
  name: string
  lhs: Pat
  rhs: Pat
  /** Learned from a piece of a program, not one of the machine's own. */
  learned: boolean
  /** What the piece took when the rule was learned: its moves (a rule it used counting as the moves that
   * rule stands for) and its ticks. */
  moves: number
  ticks: number
  /** The piece it is for (ruleKey), and that piece's program (idOf), to tell which signals carry it. */
  key: string
  pid: number
}
/** A piece learned as a rule, where and when. */
export interface Learn { t: number; rule: Rule; x: number; y: number }
/** A rule rewriting at a node: the nodes its left side matched and the parts its holes stood for. */
export interface Fire { t: number; rule: Rule; x: number; y: number; nodes: number[]; holes: number[] }

const hole = (h: number): Pat => ({ hole: h })
const nd = (kind: 'L' | 'S' | 'F', i: number | undefined, ...kids: Pat[]): Pat => ({ kind, kids, i })
const own = (name: string, lhs: Pat, rhs: Pat): Rule => ({ name, lhs, rhs, learned: false, moves: 1, ticks: 1, key: '', pid: -1 })
const [a, b, c] = [hole(0), hole(1), hole(2)]
/** The machine's own rules: the five moves, both ways, and tri's. */
export const MOVES = {
  swap: own('swap', nd('F', 0, a, b), nd('F', 0, b, a)),
  cross: own('cross', nd('F', 0, nd('F', 1, a, b), c), nd('F', 0, nd('F', 1, a, c), b)),
  sink: own('sink', nd('S', 0, nd('F', 1, a, b)), nd('F', 0, nd('S', 1, a), b)),
  rise: own('sink', nd('F', 0, nd('S', 1, a), b), nd('S', 0, nd('F', 1, a, b))),
  stem: own('stem', a, nd('S', undefined, a)),
  'stem†': own('stem†', nd('S', 0, a), a),
  unit: own('unit', a, nd('F', undefined, nd('L', undefined), a)),
  'unit†': own('unit†', nd('F', 0, nd('L', 1), a), a),
  lift: own('tri: stem off', nd('F', 0, nd('S', 1, a), b), nd('F', 0, a, b)),
  lower: own('tri: stem on', nd('F', 0, a, b), nd('F', 0, nd('S', undefined, a), b)),
  rotate: own('tri: rotate', nd('F', 0, nd('F', 1, a, b), c), nd('F', 0, a, nd('F', 1, b, c))),
  unrotate: own('tri: rotate back', nd('F', 0, a, nd('F', 1, b, c)), nd('F', 0, nd('F', 1, a, b), c))
}
type Move = keyof typeof MOVES

/** Rules by the piece of program they were learned for. */
export type Know = Map<string, Rule[]>
/** Every rule learned so far, kept with the program between runs: the rules a run starts with. With rules
 * 'shared', also the one book every node reads; with rules in cells, each cell's own rules, by the cell. */
export class Book {
  byKey: Know = new Map()
  all: Rule[] = []
  cells = new Map<string, Know>()
  private seen = new Set<string>()
  /** Adds a rule unless the piece has it already (parts of a word run the same piece side by side). */
  add(k: string, r: Rule) {
    const id = `${k}|${patKey(r.lhs)}|${patKey(r.rhs)}`
    if (this.seen.has(id)) return false
    this.seen.add(id)
    const rs = this.byKey.get(k)
    if (rs) rs.push(r)
    else this.byKey.set(k, [r])
    this.all.push(r)
    return true
  }
}
const patKey = (p: Pat): string => ('hole' in p ? `${p.hole}` : `${p.kind}${p.i ?? '+'}(${p.kids.map(patKey).join(',')})`)
const ruleId = (r: Rule) => `${r.key}|${patKey(r.lhs)}|${patKey(r.rhs)}`
const knows = (know: Know, r: Rule) => !!know.get(r.key)?.some((q) => q === r || ruleId(q) === ruleId(r))
const addTo = (know: Know, r: Rule) => know.set(r.key, [...(know.get(r.key) ?? []), r])
const copyKnow = (know: Know): Know => new Map([...know].map(([k, rs]) => [k, [...rs]]))
const patSize = (p: Pat): number => ('hole' in p ? 1 : 1 + p.kids.reduce((s, k) => s + patSize(k), 0))
/** Symbols to send a rule: which piece it is for, and its two sides. */
const ruleSize = (r: Rule) => 1 + patSize(r.lhs) + patSize(r.rhs)
const rulesSize = (rs: Iterable<Rule>) => { let s = 0; for (const r of rs) s += ruleSize(r); return s }

/** What the player replays: rewrites, node steps and signals, each at its tick. */
export type Op =
  | { t: number; k: 'node'; n: Node }
  | { t: number; k: 'drop'; id: number }
  | { t: number; k: 'kind'; id: number; kind: Kind }
  | { t: number; k: 'kids'; id: number; kids: number[] }
  | { t: number; k: 'at'; id: number; x: number; y: number }
  | { t: number; k: 'link'; l: Link }
  | { t: number; k: 'unlink'; id: number }
  | { t: number; k: 'path'; id: number; cells: Cell[] }
export interface Signal { t0: number; t1: number; cells: Cell[]; kind: 'task' | 'done' | 'probe'; label: string; size?: number }

export class Stuck extends Error {}
/** Values this small (a bit, a carry, a status) climb to whatever reads them; bigger ones stay. */
export const SMALL = 4

/** The cells of a wire from (ax, ay) to (bx, by): a step toward the child's row, across, then the rest. */
export function route(ax: number, ay: number, bx: number, by: number): Cell[] {
  const out: Cell[] = [[ax, ay]]
  let x = ax, y = ay
  const sy = Math.sign(by - ay), sx = Math.sign(bx - ax)
  if (y !== by) { y += sy; out.push([x, y]) }
  while (x !== bx) { x += sx; out.push([x, y]) }
  while (y !== by) { y += sy; out.push([x, y]) }
  return out
}

const same = (a: Cell, b: Cell) => a[0] === b[0] && a[1] === b[1]
const beside = (a: Cell, b: Cell) => Math.abs(a[0] - b[0]) + Math.abs(a[1] - b[1]) === 1
/** A path with its doublings-back taken out: a wire spliced from two that share cells runs only where it needs to. */
function cancel(p: Cell[]): Cell[] {
  const out: Cell[] = []
  for (const c of p) {
    if (out.length && same(out[out.length - 1], c)) continue
    if (out.length >= 2 && same(out[out.length - 2], c)) out.pop()
    else out.push(c)
  }
  return out
}
/** Ticks for replies leaving a cell up one wire, one a tick, from the times they are ready. */
function queue(ts: number[]): number[] {
  ts.sort((a, b) => a - b)
  for (let i = 1; i < ts.length; i++) ts[i] = Math.max(ts[i], ts[i - 1] + 1)
  return ts
}

/** A piece of the program running at a node: the nodes it has read (as they were) and made, while there
 * are few enough of them for it to be learned as a rule; and, with rules carried, the rules kept where it
 * runs and those it learned or heard of from below, which go back up with its done signal. */
interface Task {
  up: Task | null; top: number; seen: Map<number, { kind: Kind; kids: number[] }> | null; made: Set<number>; moves: number
  know: Know; got: Rule[]
}
/** What a piece sends back up: the rules learned in it, and the size of the record of what it read. */
interface Done { got: Rule[]; foot: number }
/** A rule's left side against the tree: the nodes it matched, the parts its holes stand for, every node it
 * looked at, how far each is from the top along the wires, and (wires of cells) the cells from each up to
 * the top. */
interface Match { ok: boolean; nodes: Map<number, number>; holes: Map<number, number>; read: number[]; dist: Map<number, number>; via: Map<number, Cell[]> }

export class Net {
  nodes = new Map<number, Node>()
  links = new Map<number, Link>()
  ops: Op[] = []
  signals: Signal[] = []
  fires: Fire[] = []
  learns: Learn[] = []
  now = 0
  rewrites = 0 // the machine's own rules fired
  hits = 0 // learned rules fired
  hops = 0
  steps = 0 // node steps, matter moving
  root = 0
  /** Whether nodes step to shorten their wires while the program runs. */
  relax = true
  layout: Layout = 'rows'
  /** The rules the program brings, and where learned ones are kept; none, and every piece runs by its moves. */
  book: Book | null = null
  phys: Physics = CHEAP
  peakLanes = 0 // the most wires through one cell, with wires of cells
  peakSize = 0 // the largest signal, in symbols, with sizes
  private next = 1
  private cells = new Map<string, number>()
  private parent = new Map<number, number>() // node → the link it hangs from
  private busy = new Set<number>() // nodes climbing, which relaxing leaves alone
  private occ = new Map<string, number>() // cell → wires passing through it, with wires of cells
  private kept = new Map<number, Know>() // a wire → the rules kept at its lower end, with rules carried
  private sent = new Map<number, Set<string>>() // a wire → the rules already down it, with rules carried
  peakKept = 0 // the most rule symbols kept at one place, with rules carried
  private wireFree = new Map<string, number>() // a wire and way → the tick it is free, with sizes

  // ---- building and reading ----
  /** Lays a value out under a root node: in rows, as a tidy tree with each node two rows under its parent
   * and leaves two columns apart; or packed, each part in its own share of a square, so that a word's
   * halves sit side by side, their halves one above the other, and so on (an H-tree): the distance from
   * the root to any bit grows as the square root of the word, not as the word. */
  static of(v: V, layout: Layout = 'rows', phys: Physics = CHEAP, x0 = 1, y0 = 1): Net {
    const net = new Net()
    net.layout = layout
    net.phys = phys
    if (layout === 'packed') return net.pack(v, x0, y0)
    const width = (t: V): number => (t.tag === 'leaf' ? 1 : t.tag === 'stem' ? width(t.c) : width(t.l) + width(t.r))
    net.root = net.addNode('R', x0 + width(v) - 1, y0)
    const place = (t: V, left: number, depth: number): number => {
      const w = width(t)
      const id = net.addNode(t.tag === 'leaf' ? 'L' : t.tag === 'stem' ? 'S' : 'F', x0 + left * 2 + w - 1, y0 + depth * REST)
      const kids = t.tag === 'leaf' ? [] : t.tag === 'stem' ? [place(t.c, left, depth + 1)] : [place(t.l, left, depth + 1), place(t.r, left + width(t.l), depth + 1)]
      net.setKids(id, kids.map((k) => net.addLink(id, k)))
      return id
    }
    const top = place(v, 0, 1)
    net.setKids(net.root, [net.addLink(net.root, top)])
    return net
  }
  private pack(v: V, x0: number, y0: number): Net {
    const size = (t: V): number => 1 + (t.tag === 'stem' ? size(t.c) : t.tag === 'fork' ? size(t.l) + size(t.r) : 0)
    const side = Math.ceil(Math.sqrt(2.2 * (size(v) + 1)))
    const kids = new Map<number, number[]>()
    // breadth first, so a node takes the middle of its share before its parts take theirs
    type Item = { t: V; x: number; y: number; w: number; h: number; up: number }
    this.root = this.addNode('R', x0 + Math.floor(side / 2), y0)
    let queue: Item[] = [{ t: v, x: x0, y: y0 + 1, w: side, h: side, up: this.root }]
    while (queue.length) {
      const next: Item[] = []
      for (const { t, x, y, w, h, up } of queue) {
        const id = this.addNode(t.tag === 'leaf' ? 'L' : t.tag === 'stem' ? 'S' : 'F', Math.floor(x + w / 2), Math.floor(y + h / 2))
        kids.get(up)?.push(id) ?? kids.set(up, [id])
        if (t.tag === 'stem') next.push({ t: t.c, x, y, w, h, up: id })
        else if (t.tag === 'fork') {
          const f = size(t.l) / (size(t.l) + size(t.r))
          if (w >= h) next.push({ t: t.l, x, y, w: w * f, h, up: id }, { t: t.r, x: x + w * f, y, w: w * (1 - f), h, up: id })
          else next.push({ t: t.l, x, y, w, h: h * f, up: id }, { t: t.r, x, y: y + h * f, w, h: h * (1 - f), up: id })
        }
      }
      queue = next
    }
    for (const [id, ks] of kids) this.setKids(id, ks.map((k) => this.addLink(id, k)))
    return this
  }
  /** The value as core.ts's trees. */
  value(id = this.top()): V {
    const n = this.nodes.get(id)!
    const kid = (i: number) => this.value(this.links.get(n.kids[i])!.to)
    return n.kind === 'L' ? VL() : n.kind === 'S' ? VS(kid(0)) : VF(kid(0), kid(1))
  }
  top() { return this.links.get(this.topLink())!.to }
  topLink() { return this.nodes.get(this.root)!.kids[0] }
  length(l: Link) {
    if (l.path) return l.path.length - 1
    const a = this.nodes.get(l.from)!, b = this.nodes.get(l.to)!
    return Math.abs(a.x - b.x) + Math.abs(a.y - b.y)
  }
  cellsOf(l: Link): Cell[] {
    if (l.path) return l.path
    const a = this.nodes.get(l.from)!, b = this.nodes.get(l.to)!
    return route(a.x, a.y, b.x, b.y)
  }
  /** Nodes under n, counted up to `cap`. */
  size(n: number, cap: number): number {
    let total = 1
    for (const k of this.nodes.get(n)!.kids) { if (total >= cap) break; total += this.size(this.links.get(k)!.to, cap - total) }
    return Math.min(total, cap)
  }
  /** The most wires passing between any two neighbouring cells: the lanes a link would need. With wires of
   * cells, the most passing through one cell. */
  lanes() {
    if (this.phys.wires === 'cells') return Math.max(0, ...this.occ.values())
    const lanes = new Map<string, number>()
    for (const l of this.links.values()) {
      const c = this.cellsOf(l)
      for (let i = 1; i < c.length; i++) {
        const k = [c[i - 1], c[i]].map(([x, y]) => `${x},${y}`).sort().join('|')
        lanes.set(k, (lanes.get(k) ?? 0) + 1)
      }
    }
    return Math.max(0, ...lanes.values())
  }

  // ---- logged changes ----
  private log(op: Op) { this.ops.push(op) }
  private key(x: number, y: number) { return `${x},${y}` }
  private free(x: number, y: number) { return x >= 0 && y >= 0 && !this.cells.has(this.key(x, y)) }
  /** The nearest free cell to (x, y), searching outward. */
  private near(x: number, y: number): [number, number] {
    for (let r = 0; r < 12; r++)
      for (let dy = -r; dy <= r; dy++)
        for (const dx of [r - Math.abs(dy), -(r - Math.abs(dy))]) if (this.free(x + dx, y + dy)) return [x + dx, y + dy]
    return [x, y]
  }
  addNode(kind: Kind, x: number, y: number) {
    const [fx, fy] = this.near(x, y)
    const n: Node = { id: this.next++, kind, x: fx, y: fy, kids: [] }
    this.nodes.set(n.id, n)
    this.cells.set(this.key(fx, fy), n.id)
    this.log({ t: this.now, k: 'node', n: { ...n, kids: [] } })
    return n.id
  }
  drop(id: number) {
    const n = this.nodes.get(id)!
    this.cells.delete(this.key(n.x, n.y))
    this.nodes.delete(id)
    this.log({ t: this.now, k: 'drop', id })
  }
  setKind(id: number, kind: Kind) { this.nodes.get(id)!.kind = kind; this.log({ t: this.now, k: 'kind', id, kind }) }
  setKids(id: number, kids: number[]) { this.nodes.get(id)!.kids = kids; this.log({ t: this.now, k: 'kids', id, kids: [...kids] }) }
  /** A wire from `from` to `to`; with wires of cells, along `path`, or drawn when the net is built. */
  addLink(from: number, to: number, path?: Cell[]) {
    const l: Link = { id: this.next++, from, to }
    if (this.phys.wires === 'cells') {
      const a = this.nodes.get(from)!, b = this.nodes.get(to)!
      l.path = path ?? route(a.x, a.y, b.x, b.y)
      this.occupy(l.path, 1)
    }
    this.links.set(l.id, l)
    this.parent.set(to, l.id)
    this.log({ t: this.now, k: 'link', l: { ...l } })
    return l.id
  }
  setLink(id: number, from: number, to: number, path?: Cell[]) {
    const l = this.links.get(id)!
    l.from = from; l.to = to
    if (path) { this.occupy(l.path!, -1); l.path = path; this.occupy(path, 1) }
    this.parent.set(to, id)
    this.log({ t: this.now, k: 'link', l: { ...l } })
  }
  unlink(id: number) {
    const l = this.links.get(id)!
    if (l.path) this.occupy(l.path, -1)
    this.links.delete(id)
    this.log({ t: this.now, k: 'unlink', id })
  }
  /** A wire's cells changed (paths are never changed in place: the log keeps the old ones). */
  private setPath(l: Link, path: Cell[]) {
    this.occupy(l.path!, -1)
    l.path = path
    this.occupy(path, 1)
    this.log({ t: this.now, k: 'path', id: l.id, cells: path })
  }
  private occupy(path: Cell[], d: number) {
    for (let i = 1; i < path.length - 1; i++) {
      const k = this.key(path[i][0], path[i][1])
      const n = (this.occ.get(k) ?? 0) + d
      if (n) this.occ.set(k, n)
      else this.occ.delete(k)
      if (n > this.peakLanes) this.peakLanes = n
    }
  }
  private stepTo(n: Node, x: number, y: number) {
    this.cells.delete(this.key(n.x, n.y))
    n.x = x; n.y = y
    this.cells.set(this.key(x, y), n.id)
    this.log({ t: this.now, k: 'at', id: n.id, x, y })
    this.steps++
  }
  /** How hard a node's wires pull at it, sitting at (x, y). In rows, each wire wants its child REST rows
   * under its parent, and minds rows more than columns; packed, each wire just wants to be short. */
  private strain(n: Node, x: number, y: number) {
    const [rest, stiff] = this.layout === 'rows' ? [REST, STIFF] : [0, 1]
    let e = 0
    const up = this.parent.get(n.id)
    if (up !== undefined && this.links.has(up)) { const p = this.nodes.get(this.links.get(up)!.from)!; e += Math.abs(x - p.x) + stiff * Math.abs(y - p.y - rest) }
    for (const k of n.kids) { const c = this.nodes.get(this.links.get(k)!.to)!; e += Math.abs(c.x - x) + stiff * Math.abs(c.y - y - rest) }
    return e
  }
  /** One tick of matter: each node steps to the free neighbouring cell that eases its wires most, if any does. */
  relaxTick() {
    if (this.phys.wires === 'cells') return this.relaxCells()
    for (const n of this.nodes.values()) {
      if (n.kind === 'R' || this.busy.has(n.id)) continue
      const e0 = this.strain(n, n.x, n.y)
      let best = -1, be = e0
      for (let d = 0; d < 4; d++) {
        const x = n.x + DX[d], y = n.y + DY[d]
        if (!this.free(x, y)) continue
        const e = this.strain(n, x, y)
        if (e < be) { be = e; best = d }
      }
      if (best >= 0) this.stepTo(n, n.x + DX[best], n.y + DY[best])
    }
  }
  /** The wires at a node, each with whether the node is its first cell (a wire down to a child). */
  private wiresOf(n: Node) {
    const ws = n.kids.map((k) => ({ l: this.links.get(k)!, start: true }))
    const up = this.parent.get(n.id)
    if (up !== undefined && this.links.has(up)) ws.push({ l: this.links.get(up)!, start: false })
    return ws
  }
  /** One tick of matter with wires of cells. A node sees only the first cell of each of its wires: stepping
   * onto that cell shortens that wire by one, and stepping anywhere else lengthens it by one, the old cell
   * joining it. So a node steps where more of its wires lead than not, if the cell it leaves has a lane for
   * each wire that grows. Then each wire takes out its kinks. */
  private relaxCells() {
    for (const n of this.nodes.values()) {
      if (n.kind === 'R' || this.busy.has(n.id)) continue
      const ws = this.wiresOf(n)
      if (!ws.length) continue
      const here = this.occ.get(this.key(n.x, n.y)) ?? 0
      let best = -1, bd = 0
      for (let d = 0; d < 4; d++) {
        const c: Cell = [n.x + DX[d], n.y + DY[d]]
        if (!this.free(c[0], c[1])) continue
        const grow = ws.filter(({ l, start }) => !same(c, l.path![start ? 1 : l.path!.length - 2])).length
        const delta = 2 * grow - ws.length
        if (delta < bd && here + grow <= LANES) { bd = delta; best = d }
      }
      if (best >= 0) this.stepAlong(n, [n.x + DX[best], n.y + DY[best]])
    }
    this.straighten()
  }
  /** A node steps to the next cell, each wire shortening if it ran through that cell and lengthening if not. */
  private stepAlong(n: Node, c: Cell) {
    for (const { l, start } of this.wiresOf(n)) {
      const p = l.path!
      if (start) this.setPath(l, same(p[1], c) ? p.slice(1) : [c, ...p])
      else this.setPath(l, same(p[p.length - 2], c) ? p.slice(0, -1) : [...p, c])
    }
    this.stepTo(n, c[0], c[1])
  }
  /** Each wire takes out its kinks, a few cells at a time: where it doubles back on itself, or goes three
   * cells round to a cell beside where it was (both fit in a 2×2 block), the detour goes. A cell that just
   * changed waits for the next tick. */
  private straighten() {
    for (const l of this.links.values()) {
      const p = l.path!
      if (p.length < 4) continue
      const out: Cell[] = [p[0]]
      let changed = false
      for (let i = 1; i < p.length; ) {
        const a = out[out.length - 1]
        if (i + 1 < p.length - 1 && (same(a, p[i + 1]) || beside(a, p[i + 2]))) {
          i += 2
          changed = true
          for (let j = 0; j < 2 && i < p.length; j++) out.push(p[i++])
          continue
        }
        out.push(p[i++])
      }
      if (changed) this.setPath(l, out)
    }
  }
  /** The free cell nearest a node, for a node a rewrite makes, and the cells from it back to that node. */
  private room(anchor: number): [Cell, Cell[]] {
    const a = this.nodes.get(anchor)!
    const back = new Map<string, Cell | null>([[this.key(a.x, a.y), null]])
    let ring: Cell[] = [[a.x, a.y]]
    while (ring.length) {
      const next: Cell[] = []
      for (const c of ring)
        for (let d = 0; d < 4; d++) {
          const q: Cell = [c[0] + DX[d], c[1] + DY[d]]
          const k = this.key(q[0], q[1])
          if (q[0] < 0 || q[1] < 0 || back.has(k)) continue
          back.set(k, c)
          if (this.free(q[0], q[1])) {
            const path: Cell[] = [q]
            for (let p: Cell | null = c; p; p = back.get(this.key(p[0], p[1]))!) path.push(p)
            return [q, path]
          }
          next.push(q)
        }
      ring = next
    }
    throw new Error('no free cell')
  }

  // ---- rules ----
  private nodeAt(l: number) { return this.nodes.get(this.links.get(l)!.to)! }
  /** Whether a rule's left side fits the tree wire l leads to; every node looked at goes in `read`. */
  private match(r: Rule, l: number): Match {
    const m: Match = { ok: false, nodes: new Map(), holes: new Map(), read: [], dist: new Map(), via: new Map() }
    const go = (p: Pat, id: number, d: number, via: Cell[] | null): boolean => {
      m.dist.set(id, d)
      if (via) m.via.set(id, via)
      if ('hole' in p) { m.holes.set(p.hole, id); return true }
      const n = this.nodes.get(id)!
      m.read.push(id)
      if (n.kind !== p.kind) return false
      if (p.i !== undefined) m.nodes.set(p.i, id)
      return p.kids.every((k, j) => {
        const w = this.links.get(n.kids[j])!
        return go(k, w.to, d + this.length(w), via && [...[...w.path!].reverse(), ...via.slice(1)])
      })
    }
    const top = this.nodeAt(l)
    m.ok = go(r.lhs, top.id, 0, this.phys.wires === 'cells' ? [[top.x, top.y]] : null)
    return m
  }
  /** With wires of cells, a new wire between two nodes of a rewrite: along the old wires between them. */
  private splice(m: Match, from: number, to: number) {
    return cancel([...m.via.get(from)!, ...[...m.via.get(to)!].reverse().slice(1)])
  }
  /** Rewrites the tree wire l leads to by a rule whose left side matched it: the matched nodes and the
   * holes' parts are rewired as the right side says, its new nodes made near where they go, and the
   * matched nodes it leaves out dropped. With wires of cells, a wire that changes ends runs along the old
   * wires between them, and a new node takes the free cell nearest its first part (or its parent), wired back
   * along the cells between. Returns how far the farthest node it changed is from the top, along wires. */
  private rewrite(r: Rule, l: number, m: Match, task: Task): number {
    const n = this.nodeAt(l)
    const cells = this.phys.wires === 'cells'
    this.fires.push({ t: this.now, rule: r, x: n.x, y: n.y, nodes: [...m.nodes.values()], holes: [...m.holes.keys()].sort((p, q) => p - q).map((h) => m.holes.get(h)!) })
    const old = new Map<number, number>() // a node hanging from a matched node → its wire
    for (const id of m.nodes.values()) for (const k of this.nodes.get(id)!.kids) old.set(this.links.get(k)!.to, k)
    const kept = new Set<number>(), used = new Set<number>()
    let far = 0
    const wrote = (id: number) => { far = Math.max(far, m.dist.get(id) ?? 0) }
    const placed = (p: Pat) => ('hole' in p ? this.nodes.get(m.holes.get(p.hole)!) : p.i !== undefined ? this.nodes.get(m.nodes.get(p.i) ?? -1) : undefined)
    const build = (p: Pat, x: number, y: number, parent: number | null): number => {
      if ('hole' in p) return m.holes.get(p.hole)!
      let id: number
      if (p.i !== undefined && m.nodes.has(p.i)) {
        id = m.nodes.get(p.i)!
        kept.add(id)
        if (this.nodes.get(id)!.kind !== p.kind) { this.setKind(id, p.kind); wrote(id) }
      } else {
        // a new node goes over the first of its parts that is somewhere already, or where its parent says
        const under = p.kids.map(placed).find((u) => u)
        const anchor = under ?? this.nodes.get(parent ?? n.id)!
        if (cells) {
          const [q, stub] = this.room(anchor.id)
          id = this.addNode(p.kind, q[0], q[1])
          m.via.set(id, [...stub, ...m.via.get(anchor.id)!.slice(1)])
          m.dist.set(id, (m.dist.get(anchor.id) ?? 0) + stub.length - 1)
        } else {
          id = under ? this.addNode(p.kind, under.x, under.y - 1) : this.addNode(p.kind, x, y)
          const at = this.nodes.get(id)!
          m.dist.set(id, (m.dist.get(anchor.id) ?? 0) + Math.abs(at.x - anchor.x) + Math.abs(at.y - anchor.y))
        }
        wrote(id)
        this.made(task, id)
      }
      const me = this.nodes.get(id)!
      const kids = p.kids.map((k, j) => {
        const child = build(k, me.x + (p.kids.length === 2 ? 2 * j - 1 : 0), me.y + 1, id)
        const w = old.get(child)
        if (w === undefined || used.has(w)) return this.addLink(id, child, cells ? this.splice(m, id, child) : undefined)
        used.add(w)
        if (this.links.get(w)!.from !== id) this.setLink(w, id, child, cells ? this.splice(m, id, child) : undefined)
        return w
      })
      if (kids.length !== me.kids.length || kids.some((k, j) => k !== me.kids[j])) { this.setKids(id, kids); wrote(id) }
      return id
    }
    const port = this.links.get(l)!
    const top = build(r.rhs, n.x, n.y, null)
    if (port.to !== top) this.setLink(l, port.from, top, cells ? cancel([...port.path!, ...[...m.via.get(top)!].reverse().slice(1)]) : undefined)
    for (const w of old.values()) if (!used.has(w)) this.unlink(w)
    for (const id of m.nodes.values()) if (!kept.has(id)) { wrote(id); this.drop(id) }
    return far
  }
  /** One of the machine's own rules at the node wire l leads to, in a tick. swap, cross and sink leave a
   * tree they don't fit as it is; stem† and unit† are stuck on one. */
  *move(name: Move, l: number, task: Task): Gen {
    const r = MOVES[name]
    const m = this.match(r, l)
    for (const id of m.read) this.touch(task, id)
    let far = 0
    if (m.ok) {
      far = this.rewrite(r, l, m, task)
      this.rewrites++
      for (let t: Task | null = task; t; t = t.up) t.moves++
    } else if (name === 'stem†' || name === 'unit†' || name === 'lower' || name === 'unrotate') throw new Stuck(`${r.name} does not fit here`)
    yield { wait: this.phys.writes ? Math.max(1, far) : 1 }
  }
  /** A piece of the program read a node: what it was goes into the piece and the pieces it is part of. */
  private touch(task: Task, id: number) {
    for (let t: Task | null = task; t && t.seen; t = t.up) {
      if (t.made.has(id) || t.seen.has(id)) continue
      const n = this.nodes.get(id)!
      t.seen.set(id, { kind: n.kind, kids: n.kids.map((w) => this.links.get(w)!.to) })
      if (t.seen.size + t.made.size > RULE_MAX) t.seen = null
    }
  }
  private made(task: Task, id: number) {
    for (let t: Task | null = task; t && t.seen; t = t.up) {
      t.made.add(id)
      if (t.seen.size + t.made.size > RULE_MAX) t.seen = null
    }
  }
  /** The rule a piece that has finished stands for: the nodes it read, as they were, with the parts it
   * never read as holes; and what is there now, made of those nodes, the holes, and nodes it made. */
  private learn(k: string, pid: number, t: Task, l: number, name: string, ticks: number) {
    const seen = t.seen!
    const index = new Map<number, number>(), holeOf = new Map<number, number>()
    const left = (id: number): Pat => {
      const s = seen.get(id)
      if (!s) { const h = holeOf.size; holeOf.set(id, h); return { hole: h } }
      const i = index.size
      index.set(id, i)
      return { kind: s.kind as 'L' | 'S' | 'F', i, kids: s.kids.map(left) }
    }
    const lhs = left(t.top)
    const right = (id: number): Pat | null => {
      if (seen.has(id) || t.made.has(id)) {
        const n = this.nodes.get(id)!
        const kids = n.kids.map((w) => right(this.links.get(w)!.to))
        return kids.every((x) => x) ? { kind: n.kind as 'L' | 'S' | 'F', i: index.get(id), kids: kids as Pat[] } : null
      }
      const h = holeOf.get(id)
      return h === undefined ? null : { hole: h }
    }
    const rhs = right(this.links.get(l)!.to)
    if (!rhs) return
    const rule: Rule = { name, lhs, rhs, learned: true, moves: t.moves, ticks, key: k, pid }
    const n = this.nodeAt(l)
    const fresh = this.book!.add(k, rule)
    if (this.phys.rules === 'carried') this.teach(t, [rule])
    if (this.phys.rules === 'cell') {
      const c = this.key(n.x, n.y)
      const know = this.book!.cells.get(c) ?? new Map()
      if (!knows(know, rule)) addTo(know, rule)
      this.book!.cells.set(c, know)
    }
    if (fresh) this.learns.push({ t: this.now, rule, x: n.x, y: n.y })
  }
  /** With rules carried: rules a piece learned, or heard of from a part of it, that it did not know. */
  private teach(t: Task, rules: Rule[]) {
    if (this.phys.rules !== 'carried') return
    for (const r of rules) if (!knows(t.know, r)) { addTo(t.know, r); t.got.push(r) }
  }
  /** With rules carried, the rules kept at the lower end of wire l. */
  private keptAt(l: number): Know {
    let k = this.kept.get(l)
    if (!k) this.kept.set(l, (k = new Map()))
    return k
  }
  /** The rules for piece k that a node at wire l has to go on: the shared book, those that came with the
   * piece, or those kept in the node's cell. */
  private known(k: string, l: number, know: Know): Rule[] | undefined {
    if (this.phys.rules === 'shared') return this.book!.byKey.get(k)
    if (this.phys.rules === 'carried') return know.get(k)
    const n = this.nodeAt(l)
    return this.book!.cells.get(this.key(n.x, n.y))?.get(k)
  }
  /** With rules carried, the rules that go down wire l with piece p: those for p or a part of it (or of
   * the fix it runs, if it runs self) that have not been down that wire before. */
  private kitFor(l: number, p: V, me: V | null, know: Know): Rule[] {
    const ids = subIds(p), mine = me && usesSelf(p) ? subIds(me) : null
    let sent = this.sent.get(l)
    if (!sent) this.sent.set(l, (sent = new Set()))
    const kit: Rule[] = []
    for (const rs of know.values()) {
      if (!ids.has(rs[0].pid) && !mine?.has(rs[0].pid)) continue
      for (const r of rs) if (!sent.has(ruleId(r))) { sent.add(ruleId(r)); kit.push(r) }
    }
    return kit
  }
  /** Reading the nodes a lookup looked at: a signal down each wire to them and back, all at once. With
   * sizes, each node's answer is a symbol, and answers sharing a wire on the way back go one a tick. */
  *look(l: number, read: Set<number>): Gen {
    const top = this.links.get(l)!.to
    const at = new Map<number, number>([[top, 0]])
    let far = 0
    const go = (id: number) => {
      for (const k of this.nodes.get(id)!.kids) {
        const w = this.links.get(k)!
        if (!read.has(w.to)) continue
        const d = at.get(id)! + this.length(w)
        at.set(w.to, d)
        far = Math.max(far, d)
        go(w.to)
      }
    }
    go(top)
    if (!far) return
    for (const [id, d] of at) {
      if (id === top) continue
      const cells = this.cellsOf(this.links.get(this.parent.get(id)!)!), d0 = d - (cells.length - 1)
      this.signals.push({ t0: this.now + d0, t1: this.now + d, cells, kind: 'probe', label: '' })
      this.signals.push({ t0: this.now + 2 * far - d, t1: this.now + 2 * far - d0, cells: [...cells].reverse(), kind: 'probe', label: '' })
      this.hops += 2 * (cells.length - 1)
    }
    if (!this.phys.sizes) { yield { wait: 2 * far }; return }
    const back = (id: number): number[] => {
      const ts = [at.get(id)!]
      for (const k of this.nodes.get(id)!.kids) {
        const w = this.links.get(k)!
        if (at.has(w.to)) for (const t of back(w.to)) ts.push(t + this.length(w))
      }
      return queue(ts)
    }
    let end = 0
    for (const k of this.nodes.get(top)!.kids) {
      const w = this.links.get(k)!
      if (at.has(w.to)) end = Math.max(end, back(w.to).at(-1)! + this.length(w))
    }
    yield { wait: end }
  }

  // ---- running a program: tasks as generators the scheduler steps through time ----
  /** Runs p on the value, forward or backward, from tick `this.now`; returns the tick it ends. */
  run(p: V, fwd = true) {
    const sched = new Scheduler(this)
    let running = true
    const self = this
    const carried = this.phys.rules === 'carried' && this.book
    const root: Task = { up: null, top: this.root, seen: null, made: new Set(), moves: 0, know: carried ? copyKnow(this.book!.byKey) : new Map(), got: [] }
    sched.start((function* (): Gen { yield* self.down(self.topLink(), p, fwd, null, root); running = false })())
    if (this.relax) sched.start((function* (): Gen { while (running) { self.relaxTick(); yield { wait: 1 } } })())
    sched.loop()
    return this.now
  }
  /** A signal along wire l, down or up: as many ticks as the wire is long; with sizes, plus one for each
   * symbol after the first, after any signal ahead of it on the wire the same way. */
  *travel(l: number, kind: Signal['kind'], label: string, up = false, size = 1): Gen {
    const w = this.links.get(l)!
    const cells = this.cellsOf(w)
    const n = cells.length - 1
    if (!n) return
    let t0 = this.now, t1 = this.now + n
    if (this.phys.sizes) {
      const way = `${l}${up ? '^' : 'v'}`
      t0 = Math.max(this.now, this.wireFree.get(way) ?? 0)
      this.wireFree.set(way, t0 + size)
      t1 = t0 + n + size - 1
      this.peakSize = Math.max(this.peakSize, size)
    }
    this.signals.push({ t0, t1, cells: up ? [...cells].reverse() : cells, kind, label, size })
    this.hops += n
    yield { wait: t1 - this.now }
  }
  /** Reading the node at the far end of wire l: a signal there, then either a signal back or, for a small
   * value, the value itself climbing the wire to the free cell nearest its reader, a cell a tick, so the
   * reads that follow are short; what hangs under it follows by relaxing. */
  *probe(l: number): Gen {
    yield* this.travel(l, 'probe', '?')
    const w = this.links.get(l)!
    if (!this.relax || this.length(w) <= 1 || this.size(w.to, SMALL + 1) > SMALL) { yield* this.travel(l, 'probe', '', true); return }
    const y = this.nodes.get(w.to)!
    this.busy.add(y.id)
    if (this.phys.wires === 'cells') {
      // up its wire a cell a tick, its own wires growing behind it while the cells it leaves have lanes
      while (w.path!.length > 2) {
        const c = w.path![w.path!.length - 2]
        if (!this.free(c[0], c[1]) || (this.occ.get(this.key(y.x, y.y)) ?? 0) + y.kids.length > LANES) break
        this.stepAlong(y, c)
        yield { wait: 1 }
      }
    } else {
      const cells = this.cellsOf(w)
      for (let j = cells.length - 2; j >= 1; j--) {
        const [x, yy] = cells[j]
        if (!this.free(x, yy)) break
        this.stepTo(y, x, yy)
        yield { wait: 1 }
      }
    }
    this.busy.delete(y.id)
  }
  /** Piece p sent down wire l, run there, and its done signal back. With sizes, the signal carries the
   * program (a library name is one symbol: every cell holds the library, as every cell holds the machine's
   * own rules) and, with rules carried, the rules for it; the done signal carries the rules learned below
   * and, while the piece above is still being learned, the record of what was read. */
  *down(l: number, p: V, fwd: boolean, me: V | null, up: Task): Gen {
    if (form(p).k === 'id') return // nothing to send: map(id, swap, id) touches only the middle wire
    const carried = this.phys.rules === 'carried' && this.book
    const kit = carried ? this.kitFor(l, p, me, up.know) : []
    yield* this.travel(l, 'task', label(p), false, progSize(p) + (me && usesSelf(p) ? progSize(me) : 0) + rulesSize(kit))
    if (carried) {
      const at = this.keptAt(l)
      for (const r of kit) if (!knows(at, r)) addTo(at, r)
      this.peakKept = Math.max(this.peakKept, rulesSize([...at.values()].flat()))
    }
    const done = yield* this.piece(p, l, fwd, me, up, true)
    yield* this.travel(l, 'done', '', true, 1 + (up.seen ? done.foot : 0) + rulesSize(done.got))
    if (carried) for (const r of done.got) this.sent.get(l)!.add(ruleId(r))
    this.teach(up, done.got)
  }
  /** A piece of the program at the node wire l leads to: by a rule learned for it, if one fits there;
   * else by running it, and learning it as a rule if it read and made few enough nodes. `wired`: it came
   * down wire l from its parent, rather than running where its parent runs. */
  *piece(p: V, l: number, fwd: boolean, me: V | null, up: Task, wired: boolean): Generator<Step, Done, void> {
    // b† run forward is b run backward: one set of rules for both
    for (let f = form(p); f.k === 'dag'; f = form(p)) { p = f.b; fwd = !fwd }
    const carried = this.phys.rules === 'carried'
    const know = carried ? this.keptAt(l) : up.know
    const k = this.book ? ruleKey(p, fwd, me) : ''
    const known = k ? this.known(k, l, know) : undefined
    if (known) {
      const read = new Set<number>()
      let hit: { r: Rule; m: Match } | null = null
      for (const r of known) {
        const m = this.match(r, l)
        for (const id of m.read) read.add(id)
        if (m.ok) { hit = { r, m }; break }
      }
      yield* this.look(l, read)
      // matter moved while the answers came back: the same nodes, in the cells they are in now
      if (hit) hit.m = this.match(hit.r, l)
      if (hit?.m.ok) {
        for (const id of hit.m.read) this.touch(up, id)
        const far = this.rewrite(hit.r, l, hit.m, up)
        this.hits++
        for (let t: Task | null = up; t; t = t.up) t.moves += hit.r.moves
        yield { wait: this.phys.writes ? Math.max(1, far) : 1 }
        return { got: [], foot: hit.m.read.length }
      }
    }
    const t: Task = { up, top: this.nodeAt(l).id, seen: k ? new Map() : null, made: new Set(), moves: 0, know, got: [] }
    const t0 = this.now
    yield* this.exec(p, l, fwd, me, t, true)
    if (k && t.seen && t.moves > 1) this.learn(k, idOf(p), t, l, pieceName(p, fwd, me), this.now - t0)
    if (!wired) up.got.push(...t.got) // kept in the same place already
    return { got: t.got, foot: t.seen ? t.seen.size + t.made.size : 1 }
  }
  *exec(p: V, l: number, fwd: boolean, me: V | null, task: Task, piece = false): Gen {
    const f = form(p)
    if (!piece && this.book && f.k !== 'dag' && f.k !== 'atom' && nameOf(p)) { yield* this.piece(p, l, fwd, me, task, false); return }
    switch (f.k) {
      case 'id': return
      case 'none': throw new Stuck('not a program')
      case 'dag': yield* this.exec(f.b, l, !fwd, me, task); return
      case 'seq':
        if (fwd) { yield* this.exec(f.a, l, fwd, me, task); yield* this.exec(f.b, l, fwd, me, task) }
        else { yield* this.exec(f.b, l, fwd, me, task); yield* this.exec(f.a, l, fwd, me, task) }
        return
      case 'fix': yield* this.exec(f.b, l, fwd, p, task); return
      case 'map': {
        const n = this.nodeAt(l)
        this.touch(task, n.id)
        if (n.kind === 'S') yield* this.down(n.kids[0], f.a, fwd, me, task)
        else if (n.kind === 'F') yield { all: [this.down(n.kids[0], f.b, fwd, me, task), this.down(n.kids[1], f.c, fwd, me, task)] }
        return
      }
      case 'tri': {
        const n = this.nodeAt(l)
        this.touch(task, n.id)
        if (n.kind !== 'F') return
        yield* this.probe(n.kids[0])
        const s = this.nodes.get(this.links.get(n.kids[0])!.to)!
        this.touch(task, s.id)
        if (s.kind === 'L') { yield* this.down(n.kids[1], f.a, fwd, me, task); return }
        if (s.kind === 'S') {
          yield* this.move('lift', l, task)
          yield* this.exec(f.b, l, fwd, me, task)
          yield* this.move('lower', l, task)
          return
        }
        yield* this.move('rotate', l, task)
        yield* this.exec(f.c, l, fwd, me, task)
        const x = this.nodeAt(l)
        this.touch(task, x.id)
        if (x.kind !== 'F') throw new Stuck('tri needs its branch to give a fork')
        yield* this.probe(x.kids[1])
        yield* this.move('unrotate', l, task)
        return
      }
      case 'atom': {
        if (f.n === 'self') { if (!me) throw new Stuck('self outside fix'); yield* this.exec(bodyOf(me), l, fwd, me, task); return }
        if (f.n === 'eval') throw new Stuck('eval is not run here')
        const n = f.n === 'stem' || f.n === 'unit' ? (fwd ? f.n : `${f.n}†`) : f.n
        const x = this.nodeAt(l)
        if ((n === 'cross' || n === 'sink' || n === 'unit†') && (x.kind === 'F' || x.kind === 'S')) yield* this.probe(x.kids[0])
        yield* this.move(n === 'sink' && x.kind !== 'S' ? 'rise' : (n as Move), l, task)
        return
      }
    }
  }
}

/** A short name for what a signal carries. */
export function label(p: V): string {
  const f = form(p)
  return nameOf(p) ?? (f.k === 'atom' ? f.n : f.k)
}
/** What the rules learned for a piece are called: its library name, or its text. */
export function pieceName(p: V, fwd: boolean, me: V | null): string {
  const f = form(p)
  const s = f.k === 'atom' && f.n === 'self' && me ? `self (${progLabel(me)})` : progLabel(p)
  return fwd ? s : /^[\w()]+$/.test(s) ? `${s}†` : `(${s})†`
}

const IDS = new Map<string, number>()
const idOf = (p: V) => { const k = key(p); let n = IDS.get(k); if (n === undefined) IDS.set(k, (n = IDS.size)); return n }
const SUBS = new WeakMap<V, Set<number>>()
/** The programs inside p, p among them, by idOf. */
function subIds(p: V): Set<number> {
  let s = SUBS.get(p)
  if (!s) {
    s = new Set([idOf(p)])
    for (const k of p.tag === 'stem' ? [p.c] : p.tag === 'fork' ? [p.l, p.r] : []) for (const i of subIds(k)) s.add(i)
    SUBS.set(p, s)
  }
  return s
}
const SIZES = new WeakMap<V, number>()
/** The symbols it takes to send a program: one for a library name, else one a node. */
function progSize(p: V): number {
  let n = SIZES.get(p)
  if (n === undefined) {
    n = nameOf(p) ? 1 : 1 + (p.tag === 'stem' ? progSize(p.c) : p.tag === 'fork' ? progSize(p.l) + progSize(p.r) : 0)
    SIZES.set(p, n)
  }
  return n
}
const SELF = new WeakMap<V, boolean>()
/** Whether p runs `self` outside a fix of its own, so what it does depends on the fix it is in. */
function usesSelf(p: V): boolean {
  let u = SELF.get(p)
  if (u !== undefined) return u
  const f = form(p)
  u = f.k === 'atom' ? f.n === 'self' : f.k === 'dag' ? usesSelf(f.b) : f.k === 'seq' ? usesSelf(f.a) || usesSelf(f.b) : f.k === 'tri' || f.k === 'map' ? usesSelf(f.a) || usesSelf(f.b) || usesSelf(f.c) : false
  SELF.set(p, u)
  return u
}
/** Which rules a piece may use: those learned for the same program, the same way round, in the same fix
 * if it runs self. */
function ruleKey(p: V, fwd: boolean, me: V | null) {
  return `${idOf(p)}${fwd ? '+' : '-'}${me && usesSelf(p) ? idOf(me) : ''}`
}

type Step = { wait: number } | { all: Gen[] }
type Gen = Generator<Step, void, void>
interface Proc { gen: Gen; parent: Proc | null; pending: number }

/** Steps tasks in tick order; a task waiting on its parallel parts goes on when the last of them ends. */
class Scheduler {
  private queue: { t: number; seq: number; p: Proc }[] = []
  private seq = 0
  constructor(private net: Net) {}
  start(gen: Gen) { this.push(this.net.now, { gen, parent: null, pending: 0 }) }
  private push(t: number, p: Proc) {
    this.queue.push({ t, seq: this.seq++, p })
    this.queue.sort((a, b) => b.t - a.t || b.seq - a.seq) // popped from the end: earliest first
  }
  loop() {
    while (this.queue.length) {
      const { t, p } = this.queue.pop()!
      this.net.now = t
      const r = p.gen.next()
      if (r.done) {
        const parent = p.parent
        if (parent && --parent.pending === 0) this.push(t, parent)
        continue
      }
      const step = r.value
      if ('wait' in step) this.push(t + step.wait, p)
      else {
        p.pending = step.all.length
        for (const g of step.all) this.push(t, { gen: g, parent: p, pending: 0 })
      }
    }
  }
}
