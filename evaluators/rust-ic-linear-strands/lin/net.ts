// The abstract net the rules (rules.ts) run on, as tree-net's `Net` runs its rules: agents with ports,
// a pair meeting when their principal ports (port 0) are wired together, and every pair that can meet at
// the start of a round meeting in it. Flip relabels agents one at a time (rules.ts `FLIP`).

import { ARITY, FLIP, PRODUCERS, ruleFor, type End } from './rules.ts'

export const REF = (id: number, p: number) => id * 8 + p
export const idOf = (r: number) => r >> 3
export const portOf = (r: number) => r & 7

export interface Agent {
  id: number
  tag: string
  ports: number[]
}

export class Net {
  ag = new Map<number, Agent>()
  next = 1
  meetings = 0
  rounds = 0
  stuck: string | null = null

  mk(tag: string): Agent {
    const a = { id: this.next++, tag, ports: new Array(ARITY[tag]).fill(-1) }
    this.ag.set(a.id, a)
    return a
  }
  link(x: number, y: number) {
    this.ag.get(idOf(x))!.ports[portOf(x)] = y
    this.ag.get(idOf(y))!.ports[portOf(y)] = x
  }
  partner(a: Agent): Agent | null {
    const r = a.ports[0]
    if (r < 0 || portOf(r) !== 0) return null
    return this.ag.get(idOf(r)) ?? null
  }
  pairs(): [Agent, Agent][] {
    const out: [Agent, Agent][] = []
    for (const a of this.ag.values()) {
      const b = this.partner(a)
      if (b && a.id < b.id) out.push([a, b])
    }
    return out
  }

  /** The pair meets by its rule; false (and `stuck` says why) where there is none. */
  meet(a: Agent, b: Agent): boolean {
    const [c, p] = PRODUCERS.has(a.tag) ? [b, a] : [a, b]
    if (PRODUCERS.has(c.tag) || !PRODUCERS.has(p.tag)) {
      if (c.tag !== 'Out' && p.tag !== 'Out') this.stuck = `${a.tag} and ${b.tag} face each other`
      return false
    }
    const r = ruleFor(c.tag, p.tag)
    if (!r) {
      if (c.tag !== 'Out') this.stuck = `no rule for ${c.tag} meeting ${p.tag}`
      return false
    }
    const fresh = r.fresh.map((t) => this.mk(t))
    const outside = (e: End): number => ('c' in e ? c.ports[e.c] : 'p' in e ? p.ports[e.p] : REF(fresh[e.x].id, e.q))
    const links = r.wires.map(([x, y]) => [outside(x), outside(y)] as const)
    for (const [x, y] of links)
      if ([x, y].some((z) => idOf(z) === c.id || idOf(z) === p.id)) throw new Error(`${c.tag}·${p.tag}: a wire back into the pair`)
    this.ag.delete(c.id)
    this.ag.delete(p.id)
    for (const [x, y] of links) this.link(x, y)
    this.meetings++
    return true
  }

  round(): number {
    let n = 0
    for (const [a, b] of this.pairs()) if (this.ag.has(a.id) && this.ag.has(b.id) && this.partner(a) === b && this.meet(a, b)) n++
    if (n) this.rounds++
    return n
  }
  runAll(limit = 5_000_000): boolean {
    while (this.round()) if (this.meetings > limit) return false
    return true
  }

  /** Turn the run around: each agent FLIP names becomes its partner, its ports permuted. */
  flip() {
    for (const a of [...this.ag.values()]) {
      const f = FLIP[a.tag]
      if (!f) continue
      const old = a.ports.slice()
      a.tag = f.to
      a.ports = f.perm.map((i) => old[i])
      a.ports.forEach((r, i) => {
        if (r >= 0) this.ag.get(idOf(r))!.ports[portOf(r)] = REF(a.id, i)
      })
    }
  }

  clone(): Net {
    const n = new Net()
    for (const [id, a] of this.ag) n.ag.set(id, { ...a, ports: a.ports.slice() })
    n.next = this.next
    return n
  }

  /** The net as text from the root sink, ids left out: two nets alike up to ids give the same text. */
  canon(): string {
    const out = [...this.ag.values()].find((a) => a.tag === 'Out')!
    const parts: string[] = []
    const walk = (a: Agent, from: number) => {
      parts.push(a.tag, '(')
      a.ports.forEach((r, i) => {
        if (i === from) return
        if (r < 0) parts.push('-')
        else walk(this.ag.get(idOf(r))!, portOf(r))
      })
      parts.push(')')
    }
    walk(out, -1)
    return parts.join(' ') + ` #${this.ag.size}`
  }
}
