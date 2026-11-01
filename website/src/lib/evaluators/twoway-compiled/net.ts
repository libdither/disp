// Two-way programs (../twoway/core.ts) compiled into an interaction net. The program is turned into one
// agent per part: a gate for each move, a sequence node, tri and map nodes, and self, which unrolls a copy
// of its fix's body when the run reaches it. The net is a tree from the top, and every program node faces
// the run: its principal port points the way the run is, so a node's facing is all it knows about where
// the run has got to (pointer reversal by polarity, with nothing reversed but which port is principal).
//
// The run is the value's top node itself (a root): it carries a direction, and its principal port points
// where it is going. It meets a gate and the move is done there, in one meeting when the move only
// rearranges the top. A move that has to see a level down (cross, sink, unit†, tri reading the value's
// left) reads it in a second meeting; going down into a part of the value (stem†, map, tri) makes that
// part the new top in one more (a head, until it has read the node). map's halves run as two roots.
//
// Flip turns the run around in time: each root swaps its two line ports and its direction. A reader or a
// head in the middle of a step finishes it and comes out turned; a map holding one finished half lets it
// go back down. Nothing else changes, and no step has to land first.
//
// Bounce (a root leaving a node the way it came) turns the root around in space; flip turns it around in
// time. A self's copy lasts while a run is in it, and rolls back up (erased: it is the fix's body as
// written, at rest) when the run comes out.

import { form, key, type V, L as VL, S as VS, F as VF } from '../twoway/core'
import { nameOf } from '../twoway/text'

export type Gate = 'swap' | 'cross' | 'sink' | 'stem' | 'stem†' | 'unit' | 'unit†' | 'eval'
const INVERSE: Record<Gate, Gate> = { swap: 'swap', cross: 'cross', sink: 'sink', stem: 'stem†', 'stem†': 'stem', unit: 'unit†', 'unit†': 'unit', eval: 'eval' }

/** A compiled program: a tree of parts, with each fix's body kept once under a number for self. */
export type T = (
  | { k: 'gate'; g: Gate }
  | { k: 'id' }
  | { k: 'stuck'; why: string }
  | { k: 'seq'; cs: T[] }
  | { k: 'tri' | 'map'; a: T; b: T; c: T }
  | { k: 'self'; d: number }
) & { name?: string }

const ID: T = { k: 'id' }

export class Program {
  defs: T[] = []
  /** Each fix's name in the library, if it has one, for drawing its copies. */
  defNames: string[] = []
  top: T
  private memo = new Map<string, number>()
  constructor(p: V) {
    this.top = this.compile(p, false, null)
  }
  /** p run forward (or backward, `inv`); `fix` is the body self stands for. */
  private compile(p: V, inv: boolean, fix: V | null): T {
    const t = this.part(p, inv, fix)
    const nm = nameOf(p)
    if (!nm || t.k === 'id' || t.k === 'gate') return t
    const name = inv ? (nm.endsWith('†') ? nm.slice(0, -1) : nm + '†') : nm
    if (t.k === 'self') this.defNames[t.d] ??= name
    return { ...t, name }
  }
  private part(p: V, inv: boolean, fix: V | null): T {
    const f = form(p)
    const c = (q: V) => this.compile(q, inv, fix)
    switch (f.k) {
      case 'id':
        return ID
      case 'none':
        return { k: 'stuck', why: 'no program' }
      case 'dag':
        return this.compile(f.b, !inv, fix)
      case 'seq': {
        const parts = inv ? [c(f.b), c(f.a)] : [c(f.a), c(f.b)]
        const cs = parts.flatMap((t) => (t.k === 'seq' ? t.cs : t.k === 'id' ? [] : [t]))
        return cs.length === 0 ? ID : cs.length === 1 ? cs[0] : { k: 'seq', cs }
      }
      case 'tri':
      case 'map':
        return { k: f.k, a: c(f.a), b: c(f.b), c: c(f.c) }
      case 'fix':
        return { k: 'self', d: this.def(f.b, inv) }
      case 'atom': {
        if (f.n === 'self') return fix ? { k: 'self', d: this.def(fix, inv) } : { k: 'stuck', why: 'self outside a fix' }
        const g = f.n as Gate
        return { k: 'gate', g: inv ? INVERSE[g] : g }
      }
    }
  }
  private def(b: V, inv: boolean): number {
    const k = key(b) + (inv ? '†' : '')
    let d = this.memo.get(k)
    if (d === undefined) {
      d = this.defs.length
      this.memo.set(k, d)
      this.defs.push(ID)
      this.defs[d] = this.compile(b, inv, b)
    }
    return d
  }
}

// ---- the net --------------------------------------------------------------------------------------

export type Tag =
  | 'L' | 'S' | 'F' // value nodes, principal port up
  | 'R' | 'H' | 'Rd' // the run: a root (a value's top), a head (about to make a node the top), a reader
  | 'Gate' | 'Id' | 'Stuck' | 'Self' | 'Call' | 'Seq' | 'Tri' | 'Map' // program nodes
  | 'TriR' | 'TriRC' | 'MapH' // a tri reading the value as the run opens or closes it; a map holding a half
  | 'Top' | 'E' // the top of the net; an eraser rolling a copy back up

/** `p` is the principal port. Roots, heads and readers have a direction `e` (1 forward) and `f`, a flip
 * that waits for the step to finish. `s` is a gate's move, a root's shape or a reader's step, `n` a
 * self's fix, or the parts of a tri or map that are id (1, 2, 4). */
export interface Agent {
  id: number
  tag: Tag
  p: number
  ports: number[]
  e: number
  f: number
  s: string
  n: number
  /** The library's name for the part this node is the top of, for drawing only. */
  nm?: string
}

export const RF = (id: number, p: number) => id * 64 + p
export const idOf = (r: number) => Math.floor(r / 64)
export const portOf = (r: number) => r % 64

export const CURSOR = new Set<Tag>(['R', 'H', 'Rd', 'TriR', 'TriRC'])
const PLAIN = new Set<Tag>(['L', 'S', 'F'])
const NODE = new Set<Tag>(['Gate', 'Id', 'Stuck', 'Self', 'Call', 'Seq', 'Tri', 'Map', 'MapH', 'Top'])

export class Net {
  ag = new Map<number, Agent>()
  next = 1
  meetings = 0
  erasing = 0
  rounds = 0
  stuck: string | null = null
  /** Agents in the last round's meetings, for drawing. */
  fired = new Set<number>()
  constructor(public prog: Program) {}

  add(tag: Tag, n: number, o: Partial<Agent> = {}): Agent {
    const a: Agent = { id: this.next++, tag, p: 0, ports: new Array(n).fill(-1), e: 1, f: 0, s: '', n: 0, ...o }
    this.ag.set(a.id, a)
    return a
  }
  link(x: number, y: number) {
    this.ag.get(idOf(x))!.ports[portOf(x)] = y
    this.ag.get(idOf(y))!.ports[portOf(y)] = x
  }
  far = (a: Agent, i: number) => a.ports[i]
  /** Give an agent new ports, wired to the given ends. */
  wire(a: Agent, ends: number[]) {
    a.ports = new Array(ends.length).fill(-1)
    ends.forEach((x, i) => x >= 0 && this.link(RF(a.id, i), x))
  }
  del(a: Agent) {
    this.ag.delete(a.id)
  }

  /** The tree of a compiled part, at rest (every node facing up); returns its top. */
  build(t: T): Agent {
    const a = this.node(t)
    if (t.name) a.nm = t.name
    return a
  }
  private node(t: T): Agent {
    switch (t.k) {
      case 'gate':
        return this.add('Gate', 1, { s: t.g })
      case 'id':
        return this.add('Id', 1)
      case 'stuck':
        return this.add('Stuck', 1, { s: t.why })
      case 'self':
        return this.add('Self', 1, { n: t.d })
      case 'seq': {
        if (t.cs.length > 62) {
          const h = Math.ceil(t.cs.length / 2)
          return this.build({ k: 'seq', cs: [{ k: 'seq', cs: t.cs.slice(0, h) }, { k: 'seq', cs: t.cs.slice(h) }] })
        }
        const s = this.add('Seq', t.cs.length + 1)
        t.cs.forEach((c, i) => this.link(RF(s.id, i + 1), RF(this.build(c).id, 0)))
        return s
      }
      default: {
        const ids = (t.a.k === 'id' ? 1 : 0) | (t.b.k === 'id' ? 2 : 0) | (t.c.k === 'id' ? 4 : 0)
        const x = this.add(t.k === 'tri' ? 'Tri' : 'Map', 4, { n: ids })
        ;[t.a, t.b, t.c].forEach((c, i) => this.link(RF(x.id, i + 1), RF(this.build(c).id, 0)))
        return x
      }
    }
  }
  /** A plain value tree; returns its top. */
  value(v: V): Agent {
    if (v.tag === 'leaf') return this.add('L', 1)
    if (v.tag === 'stem') {
      const s = this.add('S', 2)
      this.link(RF(s.id, 1), RF(this.value(v.c).id, 0))
      return s
    }
    const f = this.add('F', 3)
    this.link(RF(f.id, 1), RF(this.value(v.l).id, 0))
    this.link(RF(f.id, 2), RF(this.value(v.r).id, 0))
    return f
  }

  /** The other agent of an active pair. */
  partner(a: Agent): Agent | null {
    const r = a.ports[a.p]
    if (r === undefined || r < 0) return null
    const b = this.ag.get(idOf(r))
    return b && portOf(r) === b.p ? b : null
  }
  pairs(): [Agent, Agent][] {
    const out: [Agent, Agent][] = []
    for (const a of this.ag.values()) {
      const b = this.partner(a)
      if (b && a.id < b.id) out.push([a, b])
    }
    return out
  }

  /** One round: every active pair meets at once (they share no agent). Returns how many met. */
  round(): number {
    this.fired.clear()
    let n = 0
    const before = this.meetings
    for (const [a, b] of this.pairs()) {
      if (!this.ag.has(a.id) || !this.ag.has(b.id) || this.partner(a) !== b) continue
      if (this.meet(a, b)) {
        n++
        this.fired.add(a.id).add(b.id)
      }
    }
    // rounds of the run itself, not of erasers rolling up a copy beside it
    if (this.meetings > before) this.rounds++
    return n
  }
  runAll(limit = 5_000_000): boolean {
    while (this.round()) if (this.meetings + this.erasing > limit) return false
    return true
  }

  /** The finished run: a root back at the top, and nothing else to do. */
  result(): { val: V; fwd: boolean } | null {
    for (const t of this.ag.values()) {
      if (t.tag !== 'Top') continue
      const r = this.partner(t)
      if (!r || r.tag !== 'R') return null
      for (const a of this.ag.values()) if (CURSOR.has(a.tag) && a !== r) return null
      return { val: this.read(r), fwd: r.e === 1 }
    }
    return null
  }
  /** The value under a root (its kids from port 2) or a plain node (from port 1). */
  read(a: Agent): V {
    const k = a.tag === 'R' ? 2 : 1
    const kid = (i: number) => this.read(this.ag.get(idOf(a.ports[k + i]))!)
    const s = a.tag === 'R' ? a.s : a.tag
    return s === 'L' ? VL() : s === 'S' ? VS(kid(0)) : VF(kid(0), kid(1))
  }

  // ---- flip ---------------------------------------------------------------------------------------

  /** Turn the run around: roots swap their line ports and direction, readers and heads turn once their
   * step is done, and a map holding a finished half sends it back down. */
  flip() {
    for (const a of [...this.ag.values()]) {
      if (a.tag === 'R') this.turn(a, true)
      else if (a.tag === 'H' || a.tag === 'Rd' || a.tag === 'TriR' || a.tag === 'TriRC') a.f ^= 1
      else if (a.tag === 'MapH' && a.s === 'l' && !(a.n & 2)) {
        a.p = a.p === 4 ? 3 : 4
        a.e ^= 1
      }
    }
  }
  /** A root turned around: in time (flip) or only in space (bounce). */
  private turn(r: Agent, time: boolean) {
    const [x, y] = [r.ports[0], r.ports[1]]
    this.link(RF(r.id, 0), y)
    this.link(RF(r.id, 1), x)
    if (time) r.e ^= 1
  }

  // ---- the rules ----------------------------------------------------------------------------------

  private meet(a: Agent, b: Agent): boolean {
    for (const [x, y] of [
      [a, b],
      [b, a]
    ]) {
      const why = this.rule(x, y)
      if (why === undefined) continue
      if (why === true) {
        if (x.tag === 'E' || y.tag === 'E') this.erasing++
        else this.meetings++
        return true
      }
      if (why !== false) this.stuck = why
      return false
    }
    if (!(a.tag === 'Top' || b.tag === 'Top')) this.stuck = `no rule for ${a.tag} and ${b.tag}`
    return false
  }

  /** The rule for x meeting y: true when it fired, a reason when stuck, undefined when not x's to fire. */
  private rule(x: Agent, y: Agent): true | false | string | undefined {
    if (x.tag === 'E') return this.erase(x, y)
    if (x.tag === 'R' && NODE.has(y.tag) && y.tag !== 'Top') return this.atNode(x, y)
    if (PLAIN.has(y.tag)) {
      if (x.tag === 'H') return this.head(x, y)
      if (x.tag === 'Rd') return this.reader(x, y)
      if (x.tag === 'TriR') return this.triOpen(x, y)
      if (x.tag === 'TriRC') return this.triCloseC(x, y)
      if (x.tag === 'MapH' && x.p === 4) return this.relaunch(x, y)
    }
    return undefined
  }

  /** A new root with the given shape, line ends and kids. */
  private root(s: string, front: number, back: number, kids: number[], e: number, f = 0): Agent {
    const r = this.add('R', 2 + kids.length, { s, e })
    this.wire(r, [front, back, ...kids])
    if (f) this.turn(r, true)
    return r
  }
  /** A root made a plain node, under `up`. */
  private demote(r: Agent, up: number) {
    const kids = r.ports.slice(2)
    r.tag = r.s as Tag
    r.s = ''
    r.p = 0
    this.rest(r)
    this.wire(r, [up, ...kids])
  }
  /** An agent no longer part of the run: its direction is no one's to read. */
  private rest(a: Agent) {
    a.e = 1
    a.f = 0
  }
  /** Bounce: leave the node the way the root came, nothing done. */
  private bounce(r: Agent): true {
    this.turn(r, false)
    return true
  }

  private atNode(r: Agent, x: Agent): true | string {
    const P = r.ports[1]
    if (x.p === 0) {
      // from above
      switch (x.tag) {
        case 'Gate':
          return this.gate(r, x, P)
        case 'Id':
          return this.bounce(r)
        case 'Stuck':
          return x.s
        case 'Seq':
          return this.down(r, x, r.e ? 1 : x.ports.length - 1, P)
        case 'Self': {
          const body = this.build(this.prog.defs[x.n])
          x.nm ??= this.prog.defNames[x.n]
          x.tag = 'Call'
          this.wire(x, [P, RF(r.id, 1)])
          x.p = 1
          this.link(RF(r.id, 0), RF(body.id, 0))
          return true
        }
        case 'Tri': {
          if (r.s !== 'F') return this.bounce(r)
          const [A, B, C] = [1, 2, 3].map((i) => x.ports[i])
          x.tag = 'TriR'
          x.e = r.e
          x.f = 0
          this.wire(x, [P, A, B, C, r.ports[2], r.ports[3]])
          x.p = 4
          this.del(r)
          return true
        }
        case 'Map':
          return this.mapOpen(r, x, P)
      }
      return `no rule for a root at ${x.tag}`
    }
    // from below, out of part x.p
    const i = x.p
    const Ci = r.ports[1]
    const Px = x.ports[0]
    const up = () => {
      this.link(RF(x.id, i), Ci)
      x.p = 0
    }
    switch (x.tag) {
      case 'Seq': {
        const k = x.ports.length - 1
        const j = r.e ? i + 1 : i - 1
        if (j < 1 || j > k) {
          up()
          this.link(RF(r.id, 0), Px)
          this.link(RF(r.id, 1), RF(x.id, 0))
          return true
        }
        this.link(RF(x.id, i), Ci)
        const Cj = x.ports[j]
        this.link(RF(x.id, j), RF(r.id, 1))
        this.link(RF(r.id, 0), Cj)
        x.p = j
        return true
      }
      case 'Call': {
        // the run is out of the copy: it rolls back up into a self
        const er = this.add('E', 1)
        this.link(RF(er.id, 0), Ci)
        x.tag = 'Self'
        x.p = 0
        this.wire(x, [RF(r.id, 1)])
        this.link(RF(r.id, 0), Px)
        return true
      }
      case 'Tri': {
        if (i === 1) {
          up()
          return this.under(r, this.root('F', Px, RF(x.id, 0), [RF(this.add('L', 1).id, 0), -1], r.e))
        }
        if (r.s !== 'F') return 'tri: its second part did not leave a fork'
        if (i === 2) {
          up()
          const o1 = r.ports[2]
          const s = this.add('S', 2)
          this.link(RF(s.id, 1), o1)
          this.link(RF(r.id, 2), RF(s.id, 0))
          this.link(RF(r.id, 0), Px)
          this.link(RF(r.id, 1), RF(x.id, 0))
          return true
        }
        const [A, B] = [x.ports[1], x.ports[2]]
        x.tag = 'TriRC'
        x.e = r.e
        x.f = 0
        this.wire(x, [Px, A, B, Ci, r.ports[3], r.ports[2]])
        x.p = 4
        this.del(r)
        return true
      }
      case 'Map': {
        if (i === 1) {
          up()
          return this.under(r, this.root('S', Px, RF(x.id, 0), [-1], r.e))
        }
        // i === 2: the left half is back
        const [A, C] = [x.ports[1], x.ports[3]]
        x.tag = 'MapH'
        x.s = 'l'
        x.e = r.e
        this.wire(x, [Px, A, Ci, C, -1])
        x.p = 3
        this.demote(r, RF(x.id, 4))
        return true
      }
      case 'MapH': {
        const left = x.s === 'l'
        const held = x.ports[4]
        const ends = [-1, x.ports[1], x.ports[2], x.ports[3]]
        ends[i] = Ci
        x.tag = 'Map'
        x.s = ''
        x.p = 0
        this.rest(x)
        this.wire(x, ends)
        return this.under(r, this.root('F', Px, RF(x.id, 0), left ? [held, -1] : [-1, held], r.e))
      }
    }
    return `no rule for a root leaving ${x.tag}`
  }

  /** The old root made plain, in the new root's empty slot. */
  private under(r: Agent, n: Agent): true {
    this.demote(r, RF(n.id, n.ports.indexOf(-1)))
    return true
  }

  /** Down into part i of x. */
  private down(r: Agent, x: Agent, i: number, P: number): true {
    const C = x.ports[i]
    this.link(RF(x.id, 0), P)
    this.link(RF(x.id, i), RF(r.id, 1))
    this.link(RF(r.id, 0), C)
    x.p = i
    return true
  }

  /** A root at a gate: the move done to the top, then back up; or a reader or head for the rest. */
  private gate(r: Agent, g: Agent, P: number): true | string {
    const eff = r.e ? (g.s as Gate) : INVERSE[g.s as Gate]
    const G = RF(g.id, 0)
    const reader = (s: string, child: number, held: number[]) => {
      r.tag = 'Rd'
      r.s = s
      r.f = 0
      this.wire(r, [child, P, G, ...held])
      return true as const
    }
    switch (eff) {
      case 'swap':
        if (r.s === 'F') {
          const [l, rr] = [r.ports[2], r.ports[3]]
          this.link(RF(r.id, 2), rr)
          this.link(RF(r.id, 3), l)
        }
        return this.bounce(r)
      case 'eval':
        return r.s === 'F' ? 'eval is not compiled here' : this.bounce(r)
      case 'stem':
        return this.under(r, this.root('S', P, G, [-1], r.e))
      case 'unit':
        return this.under(r, this.root('F', P, G, [RF(this.add('L', 1).id, 0), -1], r.e))
      case 'stem†': {
        if (r.s !== 'S') return 'stem† needs a stem'
        const c = r.ports[2]
        r.tag = 'H'
        r.s = ''
        r.f = 0
        this.wire(r, [c, P, G])
        return true
      }
      case 'unit†':
        if (r.s !== 'F') return 'unit† needs (△, x)'
        return reader('unit†', r.ports[2], [r.ports[3]])
      case 'cross':
        return r.s === 'F' ? reader('cross', r.ports[2], [r.ports[3]]) : this.bounce(r)
      case 'sink':
        if (r.s === 'S') return reader('sinkS', r.ports[2], [])
        if (r.s === 'F') return reader('sinkF', r.ports[2], [r.ports[3]])
        return this.bounce(r)
    }
  }

  /** A reader meets the node under the top: the move is finished. */
  private reader(rd: Agent, v: Agent): true | string {
    const [front, back] = [rd.ports[1], rd.ports[2]]
    const e = rd.e
    const f = rd.f
    switch (rd.s) {
      case 'unit†': {
        if (v.tag !== 'L') return 'unit† needs (△, x)'
        this.del(v)
        rd.tag = 'H'
        rd.s = ''
        this.wire(rd, [rd.ports[3], front, back])
        return true
      }
      case 'cross': {
        const w = rd.ports[3]
        this.del(rd)
        if (v.tag === 'F') {
          const [u, vv] = [v.ports[1], v.ports[2]]
          this.link(RF(v.id, 1), u)
          this.root('F', front, back, [RF(v.id, 0), vv], e, f)
          this.link(RF(v.id, 2), w)
        } else this.root('F', front, back, [RF(v.id, 0), w], e, f)
        return true
      }
      case 'sinkS': {
        this.del(rd)
        if (v.tag === 'F') {
          const [l, r] = [v.ports[1], v.ports[2]]
          const s = this.add('S', 2)
          this.link(RF(s.id, 1), l)
          this.del(v)
          this.root('F', front, back, [RF(s.id, 0), r], e, f)
        } else this.root('S', front, back, [RF(v.id, 0)], e, f)
        return true
      }
      case 'sinkF': {
        const r = rd.ports[3]
        this.del(rd)
        if (v.tag === 'S') {
          const l = v.ports[1]
          const fk = this.add('F', 3)
          this.link(RF(fk.id, 1), l)
          this.link(RF(fk.id, 2), r)
          this.del(v)
          this.root('S', front, back, [RF(fk.id, 0)], e, f)
        } else this.root('F', front, back, [RF(v.id, 0), r], e, f)
        return true
      }
    }
    return `no reader ${rd.s}`
  }

  /** A head reads the node it is on, which becomes the top. */
  private head(h: Agent, v: Agent): true {
    const [front, back] = [h.ports[1], h.ports[2]]
    const kids = v.ports.slice(1)
    this.del(h)
    this.del(v)
    this.root(v.tag, front, back, kids, h.e, h.f)
    return true
  }

  /** Map from above: a stem goes into its first part, a fork's halves into the other two at once. */
  private mapOpen(r: Agent, x: Agent, P: number): true {
    const [A, B, C] = [x.ports[1], x.ports[2], x.ports[3]]
    const head = (v: number, into: number, at: number) => {
      const h = this.add('H', 3, { e: r.e })
      this.wire(h, [v, into, RF(x.id, at)])
    }
    if (r.s === 'L') return this.bounce(r)
    if (r.s === 'S') {
      if (x.n & 1) return this.bounce(r)
      const t = r.ports[2]
      this.del(r)
      this.link(RF(x.id, 0), P)
      x.p = 1
      head(t, A, 1)
      return true
    }
    const bid = x.n & 2
    const cid = x.n & 4
    if (bid && cid) return this.bounce(r)
    const [l, rr] = [r.ports[2], r.ports[3]]
    this.del(r)
    if (bid || cid) {
      x.tag = 'MapH'
      x.s = bid ? 'l' : 'r'
      x.e = r.e
      this.wire(x, [P, A, B, C, bid ? l : rr])
      x.p = bid ? 3 : 2
      if (bid) head(rr, C, 3)
      else head(l, B, 2)
      return true
    }
    this.link(RF(x.id, 0), P)
    x.p = 2
    head(l, B, 2)
    head(rr, C, 3)
    return true
  }

  /** A map turned around while holding its finished left half: the half goes back down. */
  private relaunch(x: Agent, v: Agent): true {
    const [P, A, B, C] = x.ports.slice(0, 4)
    const kids = v.ports.slice(1)
    this.del(v)
    const e = x.e
    x.tag = 'Map'
    x.s = ''
    x.p = 2
    this.rest(x)
    this.wire(x, [P, A, -1, C])
    this.root(v.tag, B, RF(x.id, 2), kids, e)
    return true
  }

  /** Tri back at rest or facing part p, its two value ports dropped (the others stay wired). */
  private asTri(x: Agent, p: number) {
    x.tag = 'Tri'
    this.rest(x)
    x.p = p
    x.ports = x.ports.slice(0, 4)
  }

  /** Tri reading the left of the value as the run comes in: which part runs. */
  private triOpen(x: Agent, v: Agent): true {
    const [P, A, B, C] = x.ports.slice(0, 4)
    const rest = x.ports[5]
    const [e, f] = [x.e, x.f]
    const i = v.tag === 'L' ? 1 : v.tag === 'S' ? 2 : 3
    if (x.n & (1 << (i - 1))) {
      // that part is id: the value goes back up as it came
      this.asTri(x, 0)
      this.root('F', P, RF(x.id, 0), [RF(v.id, 0), rest], e, f)
      return true
    }
    this.asTri(x, i)
    if (i === 1) {
      this.del(v)
      const h = this.add('H', 3, { e, f })
      this.wire(h, [rest, A, RF(x.id, 1)])
    } else if (i === 2) {
      const t = v.ports[1]
      this.del(v)
      this.root('F', B, RF(x.id, 2), [t, rest], e, f)
    } else {
      const [s1, s2] = [v.ports[1], v.ports[2]]
      this.link(RF(v.id, 1), s2)
      this.link(RF(v.id, 2), rest)
      this.root('F', C, RF(x.id, 3), [s1, RF(v.id, 0)], e, f)
    }
    return true
  }

  /** Tri reading the value as the run leaves its third part: ((a, b), c) put back. */
  private triCloseC(x: Agent, v: Agent): true | string {
    if (v.tag !== 'F') return 'tri: its third part did not leave (a, (b, c))'
    const P = x.ports[0]
    const o1 = x.ports[5]
    const [o2, o3] = [v.ports[1], v.ports[2]]
    const [e, f] = [x.e, x.f]
    this.asTri(x, 0)
    this.link(RF(v.id, 1), o1)
    this.link(RF(v.id, 2), o2)
    this.root('F', P, RF(x.id, 0), [RF(v.id, 0), o3], e, f)
    return true
  }

  /** An eraser takes a node at rest and goes on into its parts. */
  private erase(er: Agent, y: Agent): true | undefined {
    if (CURSOR.has(y.tag) || y.tag === 'Top') return undefined
    this.del(er)
    this.del(y)
    if (y.tag === 'E') return true
    y.ports.forEach((q, i) => {
      if (i === y.p) return
      const n = this.add('E', 1)
      this.link(RF(n.id, 0), q)
    })
    return true
  }
}

/** The net that runs p on x: the value's top as the run, going down into the program from the top. */
export function load(p: V, x: V): Net {
  const net = new Net(new Program(p))
  const top = net.add('Top', 1)
  const body = net.build(net.prog.top)
  const v = net.value(x)
  const kids = v.ports.slice(1)
  net.del(v)
  const r = net.add('R', 2 + kids.length, { s: v.tag, e: 1 })
  net.wire(r, [RF(body.id, 0), RF(top.id, 0), ...kids])
  return net
}
