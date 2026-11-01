// Two-way programs (../twoway/core.ts) as an interaction net. Programs and values are trees of the same three
// agents, leaf, stem and fork, each with its principal port up. A run is a walker: one agent that moves
// through its program a node at a time, reading each node as it reaches it (a node's form is told by its
// shape, as core.ts `form` tells it), and turns the edges it has come down so that they point back up to
// it: the path from the top of the program to the walker is pointer-reversed, and those nodes are its
// contexts. So where a run is in its program is where the walker stands in the tree; nothing counts.
// Flip turns a walker around without computing anything: a walker going down at a node becomes one going
// up from it, the other way. For the walk itself, undoing a step is flip, fire, flip, as in machine.ts.
//
// At a move, the walker turns into the value: a mover meets the value's root, rearranges it, and turns
// back. `map` splits the walker in two on a fork's halves and joins them after; `tri` reads the value's
// left part to choose; `eval` walks a program held in the value. `fix(b)` copies its body as it starts:
// the walker walks one copy and keeps the other as its environment, which `self` copies again to run, and
// copies are merged back as they finish (Dup and Mrg). A step that has to see two agents' worth of tree
// (cross, sink, unit†, map and tri reading the value, a join, a copy) takes more than one interaction;
// flip lets those land first.
//
// Every interaction is an event that remembers what it consumed, so the history can be undone exactly.

import { F as VF, L as VL, S as VS, type V } from '../twoway/core'

export type Tag =
  | 'L' | 'S' | 'F' | 'Top'
  | 'Wd' | 'Wu'
  | 'PS' | 'PFL' | 'PFR' | 'PL' | 'Kself' | 'Keval'
  | 'Mv' | 'Mvd' | 'Mv2'
  | 'Mm' | 'Mmr' | 'J' | 'J2'
  | 'Mt' | 'Mt2' | 'Mtb' | 'Mtc' | 'Mtcc' | 'Mtcc2'
  | 'Dup' | 'Mrg' | 'MrgL' | 'MrgS' | 'MrgF'

/** An agent: `e` its direction (1 forward), `k` its role or label, `m` a variant, `h` whether a walker
 * carries an environment (a fix body for `self`), `keep` ids of agents it stands for, so that they come
 * back as themselves. Ports hold `id * 8 + port`, or -1. */
export interface Agent {
  id: number
  tag: Tag
  e: number
  k: number
  m: number
  h: number
  keep: number[]
  ports: number[]
}

export const NPORTS: Record<Tag, number> = {
  L: 1, S: 2, F: 3, Top: 1,
  Wd: 4, Wu: 4,
  PS: 2, PFL: 3, PFR: 4, PL: 2, Kself: 2, Keval: 3,
  Mv: 3, Mvd: 3, Mv2: 4,
  Mm: 5, Mmr: 4, J: 3, J2: 5,
  Mt: 5, Mt2: 6, Mtb: 4, Mtc: 4, Mtcc: 4, Mtcc2: 5,
  Dup: 3, Mrg: 3, MrgL: 2, MrgS: 3, MrgF: 4
}

/** Walker roles: where in its program a walker is and what it knows there. */
export const ROLE = {
  SKEL: 0, // at a part of the sequence (or a sequence or † around parts)
  FL: 1, // at the left of a fork, to tell what the fork is
  DG: 2, // at a †'s leaf, turning to its body (or back out of it)
  SA: 3, // at a sequence's first part, going on to the second
  SB: 4, // at a sequence's first part, going to the second first (run backward)
  SC: 5, // under a stem: an instruction (self, or one stem of a move), fix or map
  // 6 to 11: under k = 2 to 7 stems, a move
  FML: 12, // at the left of a fix's or map's fork
  FX: 13, // at a fix's leaf, turning to its body (or back out of it)
  MAPL: 14 // at a map's inner fork, its left half having run
} as const
const A = (k: number) => k + 4 // the role under k stems
export const ROLE_NAMES = ['part', 'fork?', '†', ';a', ';b', 'stem?', 'swap?', 'cross?', 'sink?', 'stem?', 'unit?', 'eval?', 'fix/map?', 'fix', 'map']
export const ATOM_NAMES = ['', 'self', 'swap', 'cross', 'sink', 'stem', 'unit', 'eval']

/** Context labels: what a reversed node on a walker's path is. */
export const PS_ = { PT: 0, SQ: 1, AT: 2 } as const
export const PFL_ = { SK: 0, FM: 1, MAPS: 2, TRIA: 3 } as const
export const PFR_ = { DG: 0, SQ: 1, FX: 2, MAPB: 3, TRIB: 4, TRIC: 5 } as const

export const R = (id: number, p: number) => id * 8 + p
export const idOf = (r: number) => r >> 3
export const portOf = (r: number) => r & 7

export interface Event {
  rule: string
  ids: number[]
  before: (Agent | undefined)[]
  after: (Agent | undefined)[]
}

// ---- a rewrite under construction -----------------------------------------------------------------

type End = { hole: 0 | 1; i: number } | { made: Agent; i: number }
class Rewrite {
  made: Agent[] = []
  links: [End, End][] = []
  constructor(
    public net: Net,
    public a: Agent,
    public b: Agent
  ) {}
  A = (i: number): End => ({ hole: 0, i })
  B = (i: number): End => ({ hole: 1, i })
  mk(tag: Tag, o: { id?: number; e?: number; k?: number; m?: number; h?: number; keep?: number[] } = {}): Agent {
    const ag: Agent = {
      id: o.id ?? this.net.fresh(),
      tag,
      e: o.e ?? 0,
      k: o.k ?? 0,
      m: o.m ?? 0,
      h: o.h ?? 0,
      keep: o.keep ?? [],
      ports: new Array(NPORTS[tag]).fill(-2)
    }
    this.made.push(ag)
    return ag
  }
  /** Wire one end to another; `null` leaves a port free. */
  ln(x: End | Agent, y: End | Agent | null, xi = 0, yi = 0) {
    const ex = 'tag' in x ? { made: x, i: xi } : x
    if (y === null) {
      if (!('made' in ex)) throw new Error('a hole left free')
      ex.made.ports[ex.i] = -1
      return
    }
    const ey = 'tag' in y ? { made: y, i: yi } : y
    this.links.push([ex, ey])
  }
  /** A copy of the other agent with the same id and children, for a rule that only looks at it. */
  again(x: Agent, side: 0 | 1): Agent {
    const c = this.mk(x.tag, { id: x.id, e: x.e, k: x.k, m: x.m, h: x.h, keep: x.keep })
    for (let i = 1; i < x.ports.length; i++) this.ln(c, { hole: side, i }, i)
    return c
  }
}

// ---- the net --------------------------------------------------------------------------------------

export class Net {
  agents = new Map<number, Agent>()
  next = 1
  events: Event[] = []
  /** How many events are applied; the ones after are undone and come again going forward. */
  pos = 0
  /** Where each round began, as event numbers. */
  rounds: number[] = []
  /** Whether events are kept (for undoing); a long run can go without. */
  history = true
  fresh = () => this.next++

  add(tag: Tag, o: Partial<Agent> = {}): Agent {
    const ag: Agent = { id: this.fresh(), tag, e: 0, k: 0, m: 0, h: 0, keep: [], ports: new Array(NPORTS[tag]).fill(-1), ...o }
    this.agents.set(ag.id, ag)
    return ag
  }
  link(x: number, y: number) {
    this.agents.get(idOf(x))!.ports[portOf(x)] = y
    this.agents.get(idOf(y))!.ports[portOf(y)] = x
  }
  far(x: number): number {
    return this.agents.get(idOf(x))!.ports[portOf(x)]
  }

  /** The other agent of an active pair: the one its principal port is wired to, principal to principal. */
  partner(a: Agent): Agent | null {
    const p = a.ports[0]
    if (p < 0 || portOf(p) !== 0) return null
    return this.agents.get(idOf(p)) ?? null
  }
  /** Every active pair with a rule, each once. */
  pairs(only?: (a: Agent, b: Agent) => boolean): [Agent, Agent][] {
    const out: [Agent, Agent][] = []
    for (const a of this.agents.values()) {
      const b = this.partner(a)
      if (!b || b.id < a.id) continue
      if (only && !only(a, b)) continue
      if (ruleFor(a, b)) out.push([a, b])
    }
    return out
  }

  /** Fire one active pair: the rule's right side replaces it, an event remembering what was there. */
  fire(a: Agent, b: Agent): boolean {
    const found = ruleFor(a, b)
    if (!found) return false
    const [x, y, rule] = found
    const r = new Rewrite(this, x, y)
    if (rule(r) === false) return false
    this.commit(r, rule.name)
    return true
  }

  private commit(r: Rewrite, name: string) {
    const { a, b } = r
    const touched = new Map<number, Agent | undefined>()
    const before = (id: number) => {
      if (!touched.has(id)) touched.set(id, this.agents.get(id))
    }
    before(a.id)
    before(b.id)
    const holeFar = (e: { hole: 0 | 1; i: number }) => (e.hole ? b : a).ports[e.i]
    const used = new Set<string>()
    const resolve = (e: End): number => {
      if ('made' in e) return R(e.made.id, e.i)
      const k = `${e.hole}:${e.i}`
      if (used.has(k)) throw new Error(`${name}: hole ${k} used twice`)
      used.add(k)
      const f = holeFar(e)
      if (f >= 0 && (idOf(f) === a.id || idOf(f) === b.id)) throw new Error(`${name}: a wire from the pair to itself`)
      return f
    }
    for (const id of r.made.map((m) => m.id)) before(id)
    const sets: [number, number][] = []
    for (const [x, y] of r.links) {
      const rx = resolve(x)
      const ry = resolve(y)
      if ('made' in x) x.made.ports[x.i] = ry
      else if (rx >= 0) sets.push([rx, ry])
      if ('made' in y) y.made.ports[y.i] = rx
      else if (ry >= 0) sets.push([ry, rx])
    }
    for (const side of [0, 1] as const) {
      const ag = side ? b : a
      for (let i = 1; i < ag.ports.length; i++) if (!used.has(`${side}:${i}`) && ag.ports[i] >= 0) throw new Error(`${name}: hole ${side}:${i} left`)
    }
    for (const m of r.made) if (m.ports.includes(-2)) throw new Error(`${name}: ${m.tag} has a port left unwired`)
    for (const [x] of sets) before(idOf(x))
    this.agents.delete(a.id)
    this.agents.delete(b.id)
    for (const m of r.made) {
      if (this.agents.has(m.id)) throw new Error(`${name}: id ${m.id} taken`)
      this.agents.set(m.id, m)
    }
    for (const [x, to] of sets) {
      const ag = this.agents.get(idOf(x))!
      const c = { ...ag, ports: ag.ports.slice() }
      c.ports[portOf(x)] = to
      this.agents.set(c.id, c)
    }
    this.record(name, touched)
  }

  private record(name: string, touched: Map<number, Agent | undefined>) {
    if (!this.history) {
      this.pos++
      return
    }
    const ids = [...touched.keys()]
    this.events.length = this.pos
    if (this.rounds.length && this.rounds[this.rounds.length - 1] > this.pos) this.rounds = this.rounds.filter((r) => r <= this.pos)
    this.events.push({ rule: name, ids, before: ids.map((i) => touched.get(i)), after: ids.map((i) => this.agents.get(i)) })
    this.pos++
  }

  /** One round: every active pair fires at once (they share no agent). Returns how many fired. */
  round(only?: (a: Agent, b: Agent) => boolean): number {
    const ps = this.pairs(only)
    if (!ps.length) return 0
    this.events.length = this.pos
    this.rounds = this.rounds.filter((r) => r < this.pos)
    this.rounds.push(this.pos)
    let n = 0
    for (const [a, b] of ps) {
      // an earlier pair of the round may have rewired these agents' other ports
      const x = this.agents.get(a.id)
      const y = this.agents.get(b.id)
      if (x && y && x.ports[0] === R(y.id, 0) && this.fire(x, y)) n++
    }
    return n
  }

  undo(): boolean {
    if (!this.pos) return false
    const ev = this.events[--this.pos]
    ev.ids.forEach((id, i) => (ev.before[i] ? this.agents.set(id, ev.before[i]!) : this.agents.delete(id)))
    return true
  }
  redo(): boolean {
    if (this.pos >= this.events.length) return false
    const ev = this.events[this.pos++]
    ev.ids.forEach((id, i) => (ev.after[i] ? this.agents.set(id, ev.after[i]!) : this.agents.delete(id)))
    return true
  }
  seek(n: number) {
    while (this.pos > n && this.undo());
    while (this.pos < n && this.redo());
  }

  /** Lets steps that are half done land: copies, merges, and movers and readers in the middle of a
   * step that takes more than one interaction. */
  land(limit = 100000): number {
    let n = 0
    for (let k; (k = this.round((a, b) => ADMIN.has(a.tag) || ADMIN.has(b.tag))) && n < limit; ) n += k
    return n
  }

  /** Turn every walker around: a walker going down at a node goes up from it, the other way, and a
   * mover about to move has just moved the other way. Lands half-done steps first. */
  flip(): void {
    this.land()
    const touched = new Map<number, Agent | undefined>()
    const set = (ag: Agent) => {
      if (!touched.has(ag.id)) touched.set(ag.id, this.agents.get(ag.id))
      this.agents.set(ag.id, ag)
    }
    const all = [...this.agents.values()]
    // port swaps: ports 0 and 1 trade places on walkers and movers
    const swaps = new Map<number, Agent>()
    for (const ag of all) {
      if (ag.tag === 'Wd' || ag.tag === 'Wu' || ag.tag === 'Mv' || ag.tag === 'Mvd') {
        const tag: Tag = { Wd: 'Wu', Wu: 'Wd', Mv: 'Mvd', Mvd: 'Mv' }[ag.tag] as Tag
        swaps.set(ag.id, { ...ag, tag, e: 1 - ag.e, ports: [ag.ports[1], ag.ports[0], ...ag.ports.slice(2)] })
      }
    }
    const moved = (r: number) => (swaps.has(idOf(r)) && portOf(r) <= 1 ? R(idOf(r), 1 - portOf(r)) : r)
    for (const ag of all) {
      const base = swaps.get(ag.id) ?? ag
      const ports = base.ports.map((p) => (p < 0 ? p : moved(p)))
      if (swaps.has(ag.id) || ports.some((p, i) => p !== base.ports[i])) set({ ...base, ports })
    }
    // a join whose left half has come in lets it go again, running back
    for (const ag of [...this.agents.values()]) {
      if (ag.tag !== 'J2') continue
      const w: Agent = { id: ag.keep[1], tag: 'Wd', e: 1 - ag.e, k: ROLE.MAPL, m: 0, h: ag.h, keep: [], ports: [ag.ports[2], R(ag.id, 0), ag.ports[3], ag.ports[4]] }
      const j: Agent = { ...ag, tag: 'J', e: 1 - ag.e, keep: [ag.keep[0]], ports: [R(w.id, 1), ag.ports[1], ag.ports[0]] }
      set(w)
      set(j)
      for (const [i, p] of [[0, w.ports[0]], [2, w.ports[2]], [3, w.ports[3]]] as const) {
        if (p < 0) continue
        const o = this.agents.get(idOf(p))!
        const c = { ...o, ports: o.ports.slice() }
        c.ports[portOf(p)] = R(w.id, i)
        set(c)
      }
      for (const [i, p] of [[1, j.ports[1]], [2, j.ports[2]]] as const) {
        if (p < 0) continue
        const o = this.agents.get(idOf(p))!
        const c = { ...o, ports: o.ports.slice() }
        c.ports[portOf(p)] = R(j.id, i)
        set(c)
      }
    }
    this.events.length = this.pos
    this.rounds = this.rounds.filter((r) => r < this.pos)
    this.rounds.push(this.pos)
    this.record('flip', touched)
  }
}

/** Agents of steps that take more than one interaction: flip waits for them to land. */
export const ADMIN = new Set<Tag>(['Dup', 'Mrg', 'MrgL', 'MrgS', 'MrgF', 'Mv2', 'Mm', 'Mmr', 'Mt', 'Mt2', 'Mtb', 'Mtc', 'Mtcc', 'Mtcc2'])

// ---- loading and reading --------------------------------------------------------------------------

/** Builds a tree's agents; returns the ref of its root's principal port. */
export function build(net: Net, v: V): number {
  if (v.tag === 'leaf') return R(net.add('L').id, 0)
  if (v.tag === 'stem') {
    const s = net.add('S')
    net.link(R(s.id, 1), build(net, v.c))
    return R(s.id, 0)
  }
  const f = net.add('F')
  net.link(R(f.id, 1), build(net, v.l))
  net.link(R(f.id, 2), build(net, v.r))
  return R(f.id, 0)
}

/** The net that runs p on x: a walker at the top of p, the value hanging from it. */
export function load(p: V, x: V): Net {
  const net = new Net()
  const top = net.add('Top')
  const w = net.add('Wd', { e: 1, k: ROLE.SKEL })
  net.link(R(w.id, 0), build(net, p))
  net.link(R(w.id, 1), R(top.id, 0))
  net.link(R(w.id, 2), build(net, x))
  return net
}

/** The tree whose root's principal port is at `r`, if it is a tree through and through. */
export function readTree(net: Net, r: number, depth = 100000): V | null {
  if (r < 0 || portOf(r) !== 0 || depth <= 0) return null
  const a = net.agents.get(idOf(r))
  if (!a) return null
  if (a.tag === 'L') return VL()
  if (a.tag === 'S') {
    const c = readTree(net, a.ports[1], depth - 1)
    return c && VS(c)
  }
  if (a.tag === 'F') {
    const l = readTree(net, a.ports[1], depth - 1)
    const rr = l && readTree(net, a.ports[2], depth - 1)
    return rr && VF(l!, rr)
  }
  return null
}

/** The top walker, if the run has finished: going up from the whole program, at the top. */
export function finished(net: Net): { prog: V; val: V; fwd: boolean } | null {
  for (const t of net.agents.values()) {
    if (t.tag !== 'Top') continue
    const w = net.agents.get(idOf(t.ports[0]))
    if (!w || w.tag !== 'Wu' || w.k !== ROLE.SKEL || portOf(t.ports[0]) !== 0) return null
    const prog = readTree(net, w.ports[1])
    const val = readTree(net, w.ports[2])
    return prog && val ? { prog, val, fwd: w.e === 1 } : null
  }
  return null
}

/** Runs round by round until nothing can fire; returns the rounds run, or -1 past `limit` events. */
export function runAll(net: Net, limit = 2_000_000): number {
  let rounds = 0
  for (let k; (k = net.round()); rounds++) if (net.pos > limit) return -1
  return rounds
}

// ---- the rules ------------------------------------------------------------------------------------

type Rule = (r: Rewrite) => boolean | void

/** The rule for an active pair, with the agent it is written for first. */
export function ruleFor(x: Agent, y: Agent): [Agent, Agent, Rule] | null {
  const f = rule(x, y)
  if (f) return [x, y, f]
  const g = rule(y, x)
  if (g) return [y, x, g]
  return null
}

const tree = (t: Tag) => t === 'L' || t === 'S' || t === 'F'

function rule(a: Agent, b: Agent): Rule | null {
  switch (a.tag) {
    case 'Wd':
      return tree(b.tag) ? down(a, b) : null
    case 'Wu':
      return up(a, b)
    case 'Mv':
      return tree(b.tag) ? move(a, b) : null
    case 'Mv2':
      return tree(b.tag) ? move2(a, b) : null
    case 'Mvd':
      return b.tag === 'PL' ? moved : null
    case 'Mm':
      return tree(b.tag) ? mapValue(b) : null
    case 'Mmr':
      return b.tag === 'PFL' && b.k === PFL_.FM ? mapRight : null
    case 'Mt':
      return tree(b.tag) ? (b.tag === 'F' ? triFork : triNot) : null
    case 'Mt2':
      return tree(b.tag) ? triPick(b) : null
    case 'Mtb':
      return b.tag === 'F' ? triStemClose : null
    case 'Mtc':
      return b.tag === 'PFL' && b.k === PFL_.SK ? triForkStart : null
    case 'Mtcc':
      return b.tag === 'F' ? triForkClose1 : null
    case 'Mtcc2':
      return b.tag === 'F' ? triForkClose2 : null
    case 'Dup':
      return tree(b.tag) ? dup : null
    case 'Mrg':
      return tree(b.tag) ? mrg : null
    case 'MrgL':
      return b.tag === 'L' ? mrgL : null
    case 'MrgS':
      return b.tag === 'S' ? mrgS : null
    case 'MrgF':
      return b.tag === 'F' ? mrgF : null
  }
  return null
}

// -- the walk: each rule has its mirror, so that flip, fire, flip undoes it --

/** A new walker going down, at `node`. */
function wd(r: Rewrite, role: number, e: number, node: End, ctx: End | Agent | null, val: End | Agent | null, env: End | null, id = r.a.id, h = r.a.h, ctxPort = 0) {
  const w = r.mk('Wd', { id, e, k: role, h })
  r.ln(w, node, 0)
  r.ln(w, ctx, 1, ctxPort)
  r.ln(w, val, 2)
  r.ln(w, env, 3)
  return w
}
/** A new walker going up, carrying `focus`. */
function wu(r: Rewrite, role: number, e: number, ctx: End, focus: End | Agent, val: End | Agent | null, env: End | Agent | null, id = r.a.id, h = r.a.h) {
  const w = r.mk('Wu', { id, e, k: role, h })
  r.ln(w, ctx, 0)
  r.ln(w, focus, 1)
  r.ln(w, val, 2, 0)
  r.ln(w, env, 3, 2)
  return w
}

/** Down into a stem's child, leaving the stem reversed behind (`PS`). */
const descendS = (label: number, role: number, e?: number): Rule => (r) => {
  const ps = r.mk('PS', { id: r.b.id, k: label })
  r.ln(ps, r.A(1), 1)
  wd(r, role, e ?? r.a.e, r.B(1), ps, r.A(2), r.A(3))
}
/** Up out of a reversed stem, putting it back. */
const ascendS = (role: number, e?: number): Rule => (r) => {
  const s = r.mk('S', { id: r.b.id })
  r.ln(s, r.A(1), 1)
  wu(r, role, e ?? r.a.e, r.B(1), s, r.A(2), r.A(3))
}
/** Down into a fork's left, the fork reversed behind (`PFL`), holding the right. */
const descendFL = (label: number, role: number): Rule => (r) => {
  const c = r.mk('PFL', { id: r.b.id, k: label })
  r.ln(c, r.A(1), 1)
  r.ln(c, r.B(2), 2)
  wd(r, role, r.a.e, r.B(1), c, r.A(2), r.A(3))
}
/** Up out of a reversed fork's left, putting it back. */
const ascendFL = (role: number): Rule => (r) => {
  const f = r.mk('F', { id: r.b.id })
  r.ln(f, r.A(1), 1)
  r.ln(f, r.B(2), 2)
  wu(r, role, r.a.e, r.B(1), f, r.A(2), r.A(3))
}
/** Turn round at a node: the walker going down at it goes up from it. */
const turn = (role: number): Rule => (r) => {
  const n = r.again(r.b, 1)
  wu(r, role, r.a.e, r.A(1), n, r.A(2), r.A(3))
}
/** From a reversed fork's left to its right. */
const leftToRight = (label: number, role: number, e: number): Rule => (r) => {
  const c = r.mk('PFR', { id: r.b.id, k: label })
  r.ln(c, r.B(1), 1)
  r.ln(c, r.A(1), 2)
  r.ln(c, null, 3)
  wd(r, role, e, r.B(2), c, r.A(2), r.A(3))
}
/** From a reversed fork's right to its left. */
const rightToLeft = (label: number, role: number, e: number): Rule => (r) => {
  const c = r.mk('PFL', { id: r.b.id, k: label })
  r.ln(c, r.B(1), 1)
  r.ln(c, r.A(1), 2)
  wd(r, role, e, r.B(2), c, r.A(2), r.A(3))
}

function down(w: Agent, n: Agent): Rule | null {
  const { SKEL, FL, DG, SA, SB, SC, FML, FX, MAPL } = ROLE
  const e = w.e
  const k = w.k
  switch (n.tag) {
    case 'L':
      if (k === SKEL) return named('id', turn(SKEL))
      if (k === FL) return named('† (reading)', turn(DG))
      if (k === DG) return named('† (leaving)', turn(FL))
      if (k === FML) return named('fix (reading)', turn(FX))
      if (k === FX) return named('fix (leaving)', turn(FML))
      if (k === SC) return w.h ? selfCall : null
      if (k >= A(2) && k <= A(7)) return named(`${ATOM_NAMES[k - 4]} (turning to the value)`, atomAct)
      return null
    case 'S':
      if (k === SKEL) return named('stem: an instruction', descendS(PS_.PT, SC))
      if (k === FL) return e ? named('; (into the first part)', descendS(PS_.SQ, SKEL)) : named('; (to the second part first)', turn(SB))
      if (k === SA && !e) return named('; (into the first part)', descendS(PS_.SQ, SKEL))
      if (k === SB && e) return named('; (out)', turn(FL))
      if (k === SC) return named('counting stems', descendS(PS_.AT, A(2)))
      if (k >= A(2) && k < A(7)) return named('counting stems', descendS(PS_.AT, k + 1))
      return null
    case 'F':
      if (k === SKEL) return named('fork: reading its left', descendFL(PFL_.SK, FL))
      if (k === FL) return named('tri (turning to the value)', triAct)
      if (k === SC) return named('fix or map: reading its left', descendFL(PFL_.FM, FML))
      if (k === FML) return named('map (turning to the value)', mapAct)
      if (k === MAPL) return named('map (into the left half again)', mapLeftAgain)
      return null
  }
  return null
}

function up(w: Agent, c: Agent): Rule | null {
  const { SKEL, FL, DG, SA, SB, SC, FML, FX, MAPL } = ROLE
  const e = w.e
  const k = w.k
  switch (c.tag) {
    case 'PS':
      if (c.k === PS_.PT && k === SC) return named('stem (back up)', ascendS(SKEL))
      if (c.k === PS_.SQ && k === SKEL) return e ? named('; (first part done)', ascendS(SA)) : named('; (done)', ascendS(FL))
      if (c.k === PS_.AT && k === A(2)) return named('counting stems back', ascendS(SC))
      if (c.k === PS_.AT && k > A(2) && k <= A(7)) return named('counting stems back', ascendS(k - 1))
      return null
    case 'PFL':
      if (c.k === PFL_.SK) {
        if (k === FL) return named('fork (back up)', ascendFL(SKEL))
        if (k === DG) return named('† (into its body)', leftToRight(PFR_.DG, SKEL, 1 - e))
        if (k === SA && e) return named('; (into the second part)', leftToRight(PFR_.SQ, SKEL, 1))
        if (k === SB && !e) return named('; (into the second part)', leftToRight(PFR_.SQ, SKEL, 0))
        return null
      }
      if (c.k === PFL_.FM) {
        if (k === FML) return named('fix or map (back up)', ascendFL(SC))
        if (k === FX) return fixEnter
        return null
      }
      if (c.k === PFL_.MAPS && k === SKEL) return mapStemDone
      if (c.k === PFL_.TRIA && k === SKEL) return triLeafDone
      return null
    case 'PFR':
      if (k !== SKEL) return null
      if (c.k === PFR_.DG) return named('† (out of its body)', rightToLeft(PFL_.SK, DG, 1 - e))
      if (c.k === PFR_.SQ) return e ? named('; (second part done)', rightToLeft(PFL_.SK, SB, 1)) : named('; (back to the first part)', rightToLeft(PFL_.SK, SA, 0))
      if (c.k === PFR_.FX) return fixLeave
      if (c.k === PFR_.MAPB) return mapLeftDone
      if (c.k === PFR_.TRIB) return triStemDone
      if (c.k === PFR_.TRIC) return triForkDone
      return null
    case 'Kself':
      return k === SKEL ? selfReturn : null
    case 'Keval':
      return k === SKEL ? evalReturn : null
    case 'J':
      return k === MAPL ? mapLeftIn : null
    case 'J2':
      return k === SKEL ? mapJoin : null
  }
  return null
}

function named(name: string, f: Rule): Rule {
  Object.defineProperty(f, 'name', { value: name })
  return f
}

// -- moves: the walker turns at a move's leaf into a mover on the value, and back --

/** At a move's leaf: the leaf is reversed (`PL`) and the walker becomes a mover at the value's root. */
const atomAct: Rule = (r) => {
  const { a, b } = r
  const pl = r.mk('PL', { id: b.id, k: a.k - 4 })
  r.ln(pl, r.A(1), 1)
  const mv = r.mk('Mv', { id: a.id, e: a.e, k: a.k - 4, h: a.h })
  r.ln(mv, r.A(2), 0)
  r.ln(mv, pl, 1, 0)
  r.ln(mv, r.A(3), 2)
}
/** The mover, done, back at the move's leaf: the walker goes up. */
const moved = named('move done (back to the program)', (r) => {
  const { a, b } = r
  const l = r.mk('L', { id: b.id })
  wu(r, A(a.k), a.e, r.B(1), l, r.A(1), r.A(2))
})
/** The mover has moved: `val` is the value now. */
function done(r: Rewrite, val: End | Agent, ctx: End = r.A(1), env: End = r.A(2)) {
  const a = r.a
  const d = r.mk('Mvd', { id: a.id, e: a.e, k: a.k, h: a.h })
  r.ln(d, ctx, 0)
  r.ln(d, val, 1)
  r.ln(d, env, 2)
}

function move(mv: Agent, x: Agent): Rule | null {
  const e = mv.e
  switch (mv.k) {
    case 2:
      return named('swap', (r) => {
        if (x.tag === 'F') {
          const f = r.mk('F', { id: x.id })
          r.ln(f, r.B(2), 1)
          r.ln(f, r.B(1), 2)
          done(r, f)
        } else done(r, r.again(x, 1))
      })
    case 3:
      return named('cross (reading the root)', (r) => {
        if (x.tag !== 'F') return done(r, r.again(x, 1))
        const m2 = r.mk('Mv2', { id: mv.id, e, k: 3, h: mv.h, keep: [x.id] })
        r.ln(m2, r.B(1), 0)
        r.ln(m2, r.A(1), 1)
        r.ln(m2, r.A(2), 2)
        r.ln(m2, r.B(2), 3)
      })
    case 4:
      return named('sink (reading the root)', (r) => {
        if (x.tag === 'L') return done(r, r.again(x, 1))
        const m2 = r.mk('Mv2', { id: mv.id, e, k: 4, m: x.tag === 'S' ? 0 : 1, h: mv.h, keep: [x.id] })
        r.ln(m2, r.B(1), 0)
        r.ln(m2, r.A(1), 1)
        r.ln(m2, r.A(2), 2)
        if (x.tag === 'F') r.ln(m2, r.B(2), 3)
        else r.ln(m2, null, 3)
      })
    case 5:
      if (e)
        return named('stem', (r) => {
          const s = r.mk('S')
          r.ln(s, r.again(x, 1), 1)
          done(r, s)
        })
      return x.tag === 'S' ? named('stem†', (r) => done(r, r.B(1))) : null
    case 6:
      if (e)
        return named('unit (a leaf beside)', (r) => {
          const l = r.mk('L')
          const m2 = r.mk('Mv2', { id: mv.id, e, k: 6, h: mv.h })
          r.ln(m2, l, 0)
          r.ln(m2, r.A(1), 1)
          r.ln(m2, r.A(2), 2)
          r.ln(m2, r.again(x, 1), 3)
        })
      return x.tag === 'F'
        ? named('unit† (reading the root)', (r) => {
            const m2 = r.mk('Mv2', { id: mv.id, e, k: 6, h: mv.h, keep: [x.id] })
            r.ln(m2, r.B(1), 0)
            r.ln(m2, r.A(1), 1)
            r.ln(m2, r.A(2), 2)
            r.ln(m2, r.B(2), 3)
          })
        : null
    case 7:
      return named('eval', (r) => {
        if (x.tag !== 'F') return done(r, r.again(x, 1))
        const ke = r.mk('Keval', { id: x.id, h: mv.h })
        r.ln(ke, r.A(1), 1)
        r.ln(ke, r.A(2), 2)
        wd(r, ROLE.SKEL, e, r.B(1), ke, r.B(2), null, mv.id, 0)
      })
  }
  return null
}

function move2(m: Agent, x: Agent): Rule | null {
  switch (m.k) {
    case 3:
      return named('cross', (r) => {
        const root = r.mk('F', { id: m.keep[0] })
        if (x.tag === 'F') {
          const inner = r.mk('F', { id: x.id })
          r.ln(inner, r.B(1), 1)
          r.ln(inner, r.A(3), 2)
          r.ln(root, inner, 1)
          r.ln(root, r.B(2), 2)
        } else {
          r.ln(root, r.again(x, 1), 1)
          r.ln(root, r.A(3), 2)
        }
        done(r, root)
      })
    case 4:
      if (m.m === 0)
        return named('sink', (r) => {
          const s = r.mk('S', { id: m.keep[0] })
          if (x.tag === 'F') {
            const f = r.mk('F', { id: x.id })
            r.ln(s, r.B(1), 1)
            r.ln(f, s, 1)
            r.ln(f, r.B(2), 2)
            done(r, f)
          } else {
            r.ln(s, r.again(x, 1), 1)
            done(r, s)
          }
        })
      return named('sink', (r) => {
        const f = r.mk('F', { id: m.keep[0] })
        r.ln(f, r.A(3), 2)
        if (x.tag === 'S') {
          const s = r.mk('S', { id: x.id })
          r.ln(f, r.B(1), 1)
          r.ln(s, f, 1)
          done(r, s)
        } else {
          r.ln(f, r.again(x, 1), 1)
          done(r, f)
        }
      })
    case 6:
      if (x.tag !== 'L') return null
      return m.e
        ? named('unit', (r) => {
            const f = r.mk('F', m.keep.length ? { id: m.keep[0] } : {})
            r.ln(f, r.again(x, 1), 1)
            r.ln(f, r.A(3), 2)
            done(r, f)
          })
        : named('unit†', (r) => done(r, r.A(3)))
  }
  return null
}

// -- calls: self, eval --

/** self: the environment (the fix's body) is copied; the walker runs one copy, keeping the other. */
const selfCall = named('self (a copy of the fix body)', (r) => {
  const { a, b } = r
  const k = r.mk('Kself', { id: b.id })
  r.ln(k, r.A(1), 1)
  const d = r.mk('Dup')
  r.ln(d, r.A(3), 0)
  const w = r.mk('Wd', { id: a.id, e: a.e, k: ROLE.SKEL, h: 1 })
  r.ln(w, d, 0, 1)
  r.ln(w, k, 1, 0)
  r.ln(w, r.A(2), 2)
  r.ln(w, d, 3, 2)
})
const selfReturn = named('self (done; its copy merged back)', (r) => {
  const { a, b } = r
  const l = r.mk('L', { id: b.id })
  const m = r.mk('Mrg')
  r.ln(m, r.A(1), 0)
  r.ln(m, r.A(3), 1)
  wu(r, ROLE.SC, a.e, r.B(1), l, r.A(2), m, a.id, 1)
})
const evalReturn = named('eval (done)', (r) => {
  const { a, b } = r
  const f = r.mk('F', { id: b.id })
  r.ln(f, r.A(1), 1)
  r.ln(f, r.A(2), 2)
  const d = r.mk('Mvd', { id: a.id, e: a.e, k: 7, h: b.h })
  r.ln(d, r.B(1), 0)
  r.ln(d, f, 1)
  r.ln(d, r.B(2), 2)
})

// -- fix: the body copied as the walker goes in, merged back as it comes out --

const fixEnter = named('fix (into a copy of its body)', (r) => {
  const { a, b } = r
  const c = r.mk('PFR', { id: b.id, k: PFR_.FX, h: a.h })
  r.ln(c, r.B(1), 1)
  r.ln(c, r.A(1), 2)
  r.ln(c, r.A(3), 3)
  const d = r.mk('Dup')
  r.ln(d, r.B(2), 0)
  const w = r.mk('Wd', { id: a.id, e: a.e, k: ROLE.SKEL, h: 1 })
  r.ln(w, d, 0, 1)
  r.ln(w, c, 1, 0)
  r.ln(w, r.A(2), 2)
  r.ln(w, d, 3, 2)
})
const fixLeave = named('fix (out; the copies merged back)', (r) => {
  const { a, b } = r
  const c = r.mk('PFL', { id: b.id, k: PFL_.FM })
  r.ln(c, r.B(1), 1)
  const m = r.mk('Mrg')
  r.ln(m, r.A(1), 0)
  r.ln(m, r.A(3), 1)
  r.ln(c, m, 2, 2)
  const w = r.mk('Wd', { id: a.id, e: a.e, k: ROLE.FX, h: b.h })
  r.ln(w, r.B(2), 0)
  r.ln(w, c, 1, 0)
  r.ln(w, r.A(2), 2)
  r.ln(w, r.B(3), 3)
})

// -- map: a stem's inside, or a fork's two halves at once --

/** At map's inner fork (a, b): the walker turns to the value, holding a and b. */
const mapAct: Rule = (r) => {
  const { a, b } = r
  const m = r.mk('Mm', { id: a.id, e: a.e, h: a.h, keep: [b.id] })
  r.ln(m, r.A(2), 0)
  r.ln(m, r.A(1), 1)
  r.ln(m, r.A(3), 2)
  r.ln(m, r.B(1), 3)
  r.ln(m, r.B(2), 4)
}
function mapValue(x: Agent): Rule {
  if (x.tag === 'L')
    return named('map (a leaf: nothing inside)', (r) => {
      const { a } = r
      const fi = r.mk('F', { id: a.keep[0] })
      r.ln(fi, r.A(3), 1)
      r.ln(fi, r.A(4), 2)
      wu(r, ROLE.FML, a.e, r.A(1), fi, r.again(x, 1), r.A(2))
    })
  if (x.tag === 'S')
    return named('map (a stem: its first part runs inside)', (r) => {
      const { a } = r
      const c = r.mk('PFL', { id: a.keep[0], k: PFL_.MAPS, keep: [x.id] })
      r.ln(c, r.A(1), 1)
      r.ln(c, r.A(4), 2)
      wd(r, ROLE.SKEL, a.e, r.A(3), c, r.B(1), r.A(2))
    })
  return named('map (a fork: its halves run at once)', (r) => {
    const { a } = r
    const pb = r.mk('PFR', { id: a.keep[0], k: PFR_.MAPB })
    r.ln(pb, r.A(3), 2)
    r.ln(pb, null, 3)
    const wl = r.mk('Wd', { id: a.id, e: a.e, k: ROLE.SKEL, h: a.h })
    r.ln(wl, r.A(4), 0)
    r.ln(wl, pb, 1, 0)
    r.ln(wl, r.B(1), 2)
    const mr = r.mk('Mmr', { e: a.e, h: a.h, keep: [x.id] })
    r.ln(mr, r.A(1), 0)
    r.ln(mr, r.B(2), 1)
    r.ln(mr, pb, 3, 1)
    if (a.h) {
      const d = r.mk('Dup')
      r.ln(d, r.A(2), 0)
      r.ln(d, wl, 1, 3)
      r.ln(d, mr, 2, 2)
    } else {
      r.ln(wl, null, 3)
      r.ln(mr, null, 2)
    }
  })
}
const mapRight = named('map (the right half starts)', (r) => {
  const { a, b } = r
  const j = r.mk('J', { id: b.id, e: a.e, h: a.h, keep: [a.keep[0]] })
  r.ln(j, r.A(3), 0)
  r.ln(j, r.B(1), 1)
  wd(r, ROLE.SKEL, a.e, r.B(2), j, r.A(1), r.A(2), a.id, a.h, 2)
})
const mapStemDone = named('map (the inside has run; the stem closes)', (r) => {
  const { a, b } = r
  const fi = r.mk('F', { id: b.id })
  r.ln(fi, r.A(1), 1)
  r.ln(fi, r.B(2), 2)
  const s = r.mk('S', { id: b.keep[0] })
  r.ln(s, r.A(2), 1)
  wu(r, ROLE.FML, a.e, r.B(1), fi, s, r.A(3))
})
const mapLeftDone = named('map (the left half has run)', (r) => {
  const { a, b } = r
  const fi = r.mk('F', { id: b.id })
  r.ln(fi, r.B(2), 1)
  r.ln(fi, r.A(1), 2)
  wu(r, ROLE.MAPL, a.e, r.B(1), fi, r.A(2), r.A(3))
})
const mapLeftAgain: Rule = (r) => {
  const { a, b } = r
  const pb = r.mk('PFR', { id: b.id, k: PFR_.MAPB })
  r.ln(pb, r.A(1), 1)
  r.ln(pb, r.B(1), 2)
  r.ln(pb, null, 3)
  wd(r, ROLE.SKEL, a.e, r.B(2), pb, r.A(2), r.A(3))
}
const mapLeftIn = named('map (the left half waits for the right)', (r) => {
  const { a, b } = r
  const j2 = r.mk('J2', { id: b.id, e: a.e, h: b.h, keep: [b.keep[0], a.id] })
  r.ln(j2, r.B(2), 0)
  r.ln(j2, r.B(1), 1)
  r.ln(j2, r.A(1), 2)
  r.ln(j2, r.A(2), 3)
  r.ln(j2, r.A(3), 4)
})
const mapJoin = named('map (both halves have run; the fork closes)', (r) => {
  const { a, b } = r
  const fo = r.mk('F', { id: b.id })
  r.ln(fo, r.B(2), 1)
  r.ln(fo, r.A(1), 2)
  const fv = r.mk('F', { id: b.keep[0] })
  r.ln(fv, r.B(3), 1)
  r.ln(fv, r.A(2), 2)
  let env: Agent | null = null
  if (b.h) {
    env = r.mk('Mrg')
    r.ln(env, r.B(4), 0)
    r.ln(env, r.A(3), 1)
  }
  const w = r.mk('Wu', { id: a.id, e: a.e, k: ROLE.SC, h: b.h })
  r.ln(w, r.B(1), 0)
  r.ln(w, fo, 1)
  r.ln(w, fv, 2)
  if (env) r.ln(w, env, 3, 2)
  else r.ln(w, null, 3)
})

// -- tri: the value's left part picks a part, and goes back --

const triAct: Rule = (r) => {
  const { a, b } = r
  const m = r.mk('Mt', { id: a.id, e: a.e, h: a.h, keep: [b.id] })
  r.ln(m, r.A(2), 0)
  r.ln(m, r.A(1), 1)
  r.ln(m, r.A(3), 2)
  r.ln(m, r.B(1), 3)
  r.ln(m, r.B(2), 4)
}
const triNot = named('tri (not a fork: nothing to look at)', (r) => {
  const { a } = r
  const fi = r.mk('F', { id: a.keep[0] })
  r.ln(fi, r.A(3), 1)
  r.ln(fi, r.A(4), 2)
  wu(r, ROLE.FL, a.e, r.A(1), fi, r.again(r.b, 1), r.A(2))
})
const triFork = named('tri (a fork: reading its left)', (r) => {
  const { a, b } = r
  const m = r.mk('Mt2', { id: a.id, e: a.e, h: a.h, keep: [a.keep[0], b.id] })
  r.ln(m, r.B(1), 0)
  for (const i of [1, 2, 3, 4]) r.ln(m, r.A(i), i)
  r.ln(m, r.B(2), 5)
})
function triPick(s: Agent): Rule {
  if (s.tag === 'L')
    return named('tri (the left is △: the first part runs on the right)', (r) => {
      const { a } = r
      const c = r.mk('PFL', { id: a.keep[0], k: PFL_.TRIA, keep: [a.keep[1], s.id] })
      r.ln(c, r.A(1), 1)
      r.ln(c, r.A(4), 2)
      wd(r, ROLE.SKEL, a.e, r.A(3), c, r.A(5), r.A(2))
    })
  if (s.tag === 'S')
    return named('tri (the left is a stem: it comes off, and the second part runs)', (r) => {
      const { a } = r
      const fv = r.mk('F', { id: a.keep[1] })
      r.ln(fv, r.B(1), 1)
      r.ln(fv, r.A(5), 2)
      const c = r.mk('PFR', { id: a.keep[0], k: PFR_.TRIB, keep: [s.id] })
      r.ln(c, r.A(1), 1)
      r.ln(c, r.A(3), 2)
      r.ln(c, null, 3)
      wd(r, ROLE.SKEL, a.e, r.A(4), c, fv, r.A(2))
    })
  return named('tri (the left is a fork: it opens, and the third part runs)', (r) => {
    const { a } = r
    const fi = r.mk('F', { id: a.keep[0] })
    r.ln(fi, r.A(3), 1)
    r.ln(fi, r.A(4), 2)
    const inner = r.mk('F', { id: s.id })
    r.ln(inner, r.B(2), 1)
    r.ln(inner, r.A(5), 2)
    const fv = r.mk('F', { id: a.keep[1] })
    r.ln(fv, r.B(1), 1)
    r.ln(fv, inner, 2)
    const m = r.mk('Mtc', { id: a.id, e: a.e, h: a.h })
    r.ln(m, r.A(1), 0)
    r.ln(m, fi, 1)
    r.ln(m, r.A(2), 2)
    r.ln(m, fv, 3)
  })
}
const triLeafDone = named('tri (the first part has run; the △ goes back)', (r) => {
  const { a, b } = r
  const fi = r.mk('F', { id: b.id })
  r.ln(fi, r.A(1), 1)
  r.ln(fi, r.B(2), 2)
  const l = r.mk('L', { id: b.keep[1] })
  const fv = r.mk('F', { id: b.keep[0] })
  r.ln(fv, l, 1)
  r.ln(fv, r.A(2), 2)
  wu(r, ROLE.FL, a.e, r.B(1), fi, fv, r.A(3))
})
const triStemDone = named('tri (the second part has run; reading what it gave)', (r) => {
  const { a, b } = r
  const fi = r.mk('F', { id: b.id })
  r.ln(fi, r.B(2), 1)
  r.ln(fi, r.A(1), 2)
  const m = r.mk('Mtb', { id: a.id, e: a.e, h: a.h, keep: [b.keep[0]] })
  r.ln(m, r.A(2), 0)
  r.ln(m, r.B(1), 1)
  r.ln(m, r.A(3), 2)
  r.ln(m, fi, 3)
})
const triStemClose = named('tri (the stem goes back on the left)', (r) => {
  const { a, b } = r
  const s = r.mk('S', { id: a.keep[0] })
  r.ln(s, r.B(1), 1)
  const f = r.mk('F', { id: b.id })
  r.ln(f, s, 1)
  r.ln(f, r.B(2), 2)
  wu(r, ROLE.FL, a.e, r.A(1), r.A(3), f, r.A(2))
})
const triForkStart = named('tri (the third part starts)', (r) => {
  const { a, b } = r
  const c = r.mk('PFR', { id: b.id, k: PFR_.TRIC })
  r.ln(c, r.B(1), 1)
  r.ln(c, r.A(1), 2)
  r.ln(c, null, 3)
  wd(r, ROLE.SKEL, a.e, r.B(2), c, r.A(3), r.A(2))
})
const triForkDone = named('tri (the third part has run; reading what it gave)', (r) => {
  const { a, b } = r
  const fo = r.mk('F', { id: b.id })
  r.ln(fo, r.B(2), 1)
  r.ln(fo, r.A(1), 2)
  const m = r.mk('Mtcc', { id: a.id, e: a.e, h: a.h })
  r.ln(m, r.A(2), 0)
  r.ln(m, r.B(1), 1)
  r.ln(m, r.A(3), 2)
  r.ln(m, fo, 3)
})
const triForkClose1 = named('tri (reading what it gave)', (r) => {
  const { a, b } = r
  const m = r.mk('Mtcc2', { id: a.id, e: a.e, h: a.h, keep: [b.id] })
  r.ln(m, r.B(2), 0)
  for (const i of [1, 2, 3]) r.ln(m, r.A(i), i)
  r.ln(m, r.B(1), 4)
})
const triForkClose2 = named('tri (the left fork closes again)', (r) => {
  const { a, b } = r
  const inner = r.mk('F', { id: b.id })
  r.ln(inner, r.A(4), 1)
  r.ln(inner, r.B(1), 2)
  const outer = r.mk('F', { id: a.keep[0] })
  r.ln(outer, inner, 1)
  r.ln(outer, r.B(2), 2)
  wu(r, ROLE.SKEL, a.e, r.A(1), r.A(3), outer, r.A(2))
})

// -- copying and merging trees, a node at a time --

const dup = named('copy a node', (r) => {
  const { a, b } = r
  const one = r.mk(b.tag, { id: b.id })
  const two = r.mk(b.tag)
  r.ln(one, r.A(1), 0)
  r.ln(two, r.A(2), 0)
  for (let i = 1; i < b.ports.length; i++) {
    const d = r.mk('Dup', i === 1 ? { id: a.id } : {})
    r.ln(d, r.B(i), 0)
    r.ln(d, one, 1, i)
    r.ln(d, two, 2, i)
  }
})
const mrg = named('merge: reading the first copy', (r) => {
  const { a, b } = r
  const tag: Tag = b.tag === 'L' ? 'MrgL' : b.tag === 'S' ? 'MrgS' : 'MrgF'
  const m = r.mk(tag, { id: a.id, keep: [b.id] })
  r.ln(m, r.A(1), 0)
  for (let i = 1; i < b.ports.length; i++) r.ln(m, r.B(i), i)
  r.ln(m, r.A(2), b.ports.length)
})
const mrgL = named('merge a leaf', (r) => {
  const l = r.mk('L', { id: r.a.keep[0] })
  r.ln(l, r.A(1), 0)
})
const mrgS = named('merge a stem', (r) => {
  const s = r.mk('S', { id: r.a.keep[0] })
  r.ln(s, r.A(2), 0)
  const m = r.mk('Mrg', { id: r.a.id })
  r.ln(m, r.A(1), 0)
  r.ln(m, r.B(1), 1)
  r.ln(m, s, 2, 1)
})
const mrgF = named('merge a fork', (r) => {
  const f = r.mk('F', { id: r.a.keep[0] })
  r.ln(f, r.A(3), 0)
  const ml = r.mk('Mrg', { id: r.a.id })
  r.ln(ml, r.A(1), 0)
  r.ln(ml, r.B(1), 1)
  r.ln(ml, f, 2, 1)
  const mr = r.mk('Mrg')
  r.ln(mr, r.A(2), 0)
  r.ln(mr, r.B(2), 1)
  r.ln(mr, f, 2, 2)
})

