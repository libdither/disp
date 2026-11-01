// The linear two-way net's rules, as data: the website's two-way programs (website/src/lib/evaluators/twoway/core.ts)
// compiled into agents of at most 4 ports, every rule a template of the strand lattice's kind (tree-net
// rules.rs `Rule`): a consumer and a producer meet at their principal ports (port 0) and are replaced by
// a few fresh agents, wired among themselves and to the pair's other ports. No rule copies or erases.
//
// The value's top node is the run (a root: its two line ports, then its kids), and every program node
// faces the run: its principal port is the one toward where the run is. The program is a tree, so a
// part is entered and left through the same wire. Flip (`FLIP`) is a relabelling of single agents: a
// root, head or reader swaps its two line ports and its direction; a tri reading the value finishes,
// then turns; a par holding a finished half sends it back down.
//
// gen-rust.mts writes these into the copied tree-net's rules.rs; net.ts runs them as they are.

export type End = { c: number } | { p: number } | { x: number; q: number }
export interface Rule {
  consumer: string
  producer: string
  fresh: string[]
  wires: [End, End][]
}

export const ARITY: Record<string, number> = { L: 1, S: 2, F: 3, Out: 1 }
/** Producers: values and roots. Everything else consumes. */
export const PRODUCERS = new Set<string>(['L', 'S', 'F'])
/** What each kind of agent is, for the player and for reading. */
export const DESC: Record<string, string> = {}
/** Flip, agent by agent: the tag it becomes, and for each of its new ports the old port there. */
export const FLIP: Record<string, { to: string; perm: number[] }> = {}

const SHAPES = ['L', 'S', 'F'] as const
type Shape = (typeof SHAPES)[number]
const NKIDS: Record<Shape, number> = { L: 0, S: 1, F: 2 }
const DIRS = ['f', 'b'] as const
type Dir = (typeof DIRS)[number]
const other = (e: Dir): Dir => (e === 'f' ? 'b' : 'f')
const swapPerm = (n: number, i: number, j: number) => Array.from({ length: n }, (_, k) => (k === i ? j : k === j ? i : k))

function tag(name: string, arity: number, desc: string, producer = false) {
  ARITY[name] = arity
  DESC[name] = desc
  if (producer) PRODUCERS.add(name)
}

// ---- the agents ---------------------------------------------------------------------------------

/** A root: the value's top node, carrying the run. [front (principal), back, kids…] */
export const R = (s: Shape, e: Dir) => `R${s}${e}`
for (const s of SHAPES)
  for (const e of DIRS) {
    tag(R(s, e), 2 + NKIDS[s], `the run, going ${e === 'f' ? 'forward' : 'backward'}: the value's top, a ${{ L: 'leaf', S: 'stem', F: 'fork' }[s]}`, true)
    FLIP[R(s, e)] = { to: R(s, other(e)), perm: swapPerm(2 + NKIDS[s], 0, 1) }
  }
/** A head: about to make the plain node it faces the value's top. [v, front, back] */
export const H = (e: Dir) => `H${e}`
/** Readers finishing a move by reading the node under the top. [child, front, back, held?] */
const READERS = { Ku: ['unit†', 4], Kc: ['cross', 4], Kss: ['sink, on a stem', 3], Ksf: ['sink, on a fork', 4] } as const
for (const e of DIRS) {
  tag(H(e), 3, 'a head: makes the node it faces the top of the value, and the run goes on')
  FLIP[H(e)] = { to: H(other(e)), perm: swapPerm(3, 1, 2) }
  for (const [k, [what, n]] of Object.entries(READERS)) {
    tag(`${k}${e}`, n, `a reader finishing ${what}: it reads the node under the top`)
    FLIP[`${k}${e}`] = { to: `${k}${other(e)}`, perm: swapPerm(n, 1, 2) }
  }
}

export const GATES = ['swap', 'cross', 'sink', 'stem', 'stem†', 'unit', 'unit†'] as const
export type Gate = (typeof GATES)[number]
export const INVERSE: Record<Gate, Gate> = { swap: 'swap', cross: 'cross', sink: 'sink', stem: 'stem†', 'stem†': 'stem', unit: 'unit†', 'unit†': 'unit' }
export const GATE_TAG: Record<Gate, string> = { swap: 'Swap', cross: 'Cross', sink: 'Sink', stem: 'Stem', 'stem†': 'Unstem', unit: 'Unit', 'unit†': 'Ununit' }
for (const g of GATES) tag(GATE_TAG[g], 1, `the move ${g}`)
tag('Id', 1, 'id: the run comes back as it went')
tag('Gap', 1, 'a self the run never reaches, so never unrolled')
// a sequence of two parts, facing its parent, its first part or its second
tag('Seq0', 3, 'a; b, at rest')
tag('SeqA', 3, 'a; b, running a')
tag('SeqB', 3, 'a; b, running b')
// tri(a, b, c) is three tris of one part each: on a leaf, a stem or a fork at the value's left
const TRIS = { TL: 'a leaf', TS: 'a stem', TF: 'a fork' } as const
type Tri = keyof typeof TRIS
for (const [t, what] of Object.entries(TRIS)) {
  tag(`${t}0`, 2, `tri on ${what} at the value's left, at rest`)
  tag(`${t}i`, 2, `tri on ${what} at the value's left, running its part`)
  for (const e of DIRS)
    for (const p of ['', 'p']) {
      tag(`${t}r${e}${p}`, 4, `tri reading the value's left, to tell whether it is ${what}${p ? ', then turning' : ''}`)
      FLIP[`${t}r${e}${p}`] = { to: `${t}r${e}${p ? '' : 'p'}`, perm: [0, 1, 2, 3] }
    }
}
for (const e of DIRS)
  for (const p of ['', 'p']) {
    tag(`TFc${e}${p}`, 4, `tri on a fork, putting ((a, b), c) back as the run leaves${p ? ', then turning' : ''}`)
    FLIP[`TFc${e}${p}`] = { to: `TFc${e}${p ? '' : 'p'}`, perm: [0, 1, 2, 3] }
  }
// map(a, b, c) is map(a, id, id); map(id, b, c): on a stem, and on a fork's halves at once
tag('M0', 2, 'map on a stem, at rest')
tag('Mi', 2, 'map on a stem, running on its child')
tag('P0', 3, 'map on a fork: its halves at once, at rest')
tag('PB', 3, 'map on a fork, its halves running')
for (const e of DIRS) {
  tag(`PHC${e}`, 4, 'map on a fork, holding its finished left half for the right')
  tag(`PHR${e}`, 4, 'map on a fork, turned round: its finished left half goes back down')
  FLIP[`PHC${e}`] = { to: `PHR${other(e)}`, perm: [3, 1, 2, 0] }
  FLIP[`PHR${e}`] = { to: `PHC${other(e)}`, perm: [3, 1, 2, 0] }
}
tag('Fst0', 2, 'map on a fork, its left half only, at rest')
tag('FstB', 3, 'map on a fork, its left half running, the right held')
tag('Snd0', 2, 'map on a fork, its right half only, at rest')
tag('SndC', 3, 'map on a fork, its right half running, the left held')

// ---- building rules -----------------------------------------------------------------------------

export const RULES: Rule[] = []
class B {
  fresh: string[] = []
  wires: [End, End][] = []
  mk(t: string): number {
    if (!(t in ARITY)) throw new Error(`no agent ${t}`)
    this.fresh.push(t)
    return this.fresh.length - 1
  }
  /** Wire port q of fresh agent x to an end. */
  w(x: number, q: number, to: End) {
    this.wires.push([{ x, q }, to])
  }
  X = (x: number, q: number): End => ({ x, q })
  C = (i: number): End => ({ c: i })
  P = (i: number): End => ({ p: i })
  /** A plain node of shape s, its kids wired to `kids`; returns its port 0. */
  plain(s: Shape, kids: End[]): End {
    const n = this.mk(s)
    kids.forEach((k, i) => this.w(n, i + 1, k))
    return this.X(n, 0)
  }
  /** A root of shape s going e, front, back and kids wired to the given ends; `turned` flips it as made. */
  root(s: Shape, e: Dir, front: End, back: End, kids: End[], turned = false): number {
    const [t, f, b] = turned ? [R(s, other(e)), back, front] : [R(s, e), front, back]
    const r = this.mk(t)
    this.w(r, 0, f)
    this.w(r, 1, b)
    kids.forEach((k, i) => this.w(r, 2 + i, k))
    return r
  }
  head(e: Dir, v: End, front: End, back: End, turned = false): number {
    const [t, f, b] = turned ? [H(other(e)), back, front] : [H(e), front, back]
    const h = this.mk(t)
    this.w(h, 0, v)
    this.w(h, 1, f)
    this.w(h, 2, b)
    return h
  }
}
const ends = (n: number, from: number, f: (i: number) => End) => Array.from({ length: n }, (_, i) => f(from + i))

function rule(consumer: string, producer: string, build: (b: B) => void) {
  const b = new B()
  build(b)
  // wires are written once from a fresh port; a wire between two fresh ports is written from both, so drop the mirror
  const seen = new Set<string>()
  const wires: [End, End][] = []
  const k = (e: End) => JSON.stringify(e)
  for (const [x, y] of b.wires) {
    if (seen.has(k(y) + k(x))) continue
    seen.add(k(x) + k(y))
    wires.push([x, y])
  }
  const r: Rule = { consumer, producer, fresh: b.fresh, wires }
  validate(r)
  RULES.push(r)
}

/** Every port of the fresh agents and every aux port of the pair is wired exactly once. */
function validate(r: Rule) {
  const used = new Map<string, number>()
  const need: string[] = []
  r.fresh.forEach((t, x) => {
    for (let q = 0; q < ARITY[t]; q++) need.push(`x${x}.${q}`)
  })
  for (let i = 1; i < ARITY[r.consumer]; i++) need.push(`c${i}`)
  for (let i = 1; i < ARITY[r.producer]; i++) need.push(`p${i}`)
  const name = (e: End) => ('x' in e ? `x${e.x}.${e.q}` : 'c' in e ? `c${e.c}` : `p${e.p}`)
  for (const [a, b] of r.wires) for (const e of [a, b]) used.set(name(e), (used.get(name(e)) ?? 0) + 1)
  for (const n of need) if (used.get(n) !== 1) throw new Error(`${r.consumer}·${r.producer}: ${n} wired ${used.get(n) ?? 0} times`)
  for (const n of used.keys()) if (!need.includes(n)) throw new Error(`${r.consumer}·${r.producer}: ${n} is no port`)
}

// ---- the rules ----------------------------------------------------------------------------------

const roots = function* () {
  for (const s of SHAPES) for (const e of DIRS) yield [s, e] as const
}
/** A root's kids, as producer ends. */
const kidsP = (b: B, s: Shape) => ends(NKIDS[s], 2, b.P)

/** The run leaves the node the way it came, nothing done: node `t` rests again under the root. */
function bounce(t: string) {
  for (const [s, e] of roots())
    rule(t, R(s, e), (b) => {
      const n = b.mk(t)
      for (let i = 1; i < ARITY[t]; i++) b.w(n, i, b.C(i))
      b.root(s, e, b.P(1), b.X(n, 0), kidsP(b, s))
    })
}

// gates: the move done to the top, then back up; or a head or reader for the rest
for (const g of GATES) {
  const t = GATE_TAG[g]
  for (const [s, e] of roots()) {
    const eff = e === 'f' ? g : INVERSE[g]
    const P = R(s, e)
    const up = (b: B) => b.X(b.mk(t), 0)
    switch (eff) {
      case 'swap':
        rule(t, P, (b) => b.root(s, e, b.P(1), up(b), s === 'F' ? [b.P(3), b.P(2)] : kidsP(b, s)))
        break
      case 'stem':
        rule(t, P, (b) => b.root('S', e, b.P(1), up(b), [b.plain(s, kidsP(b, s))]))
        break
      case 'unit':
        rule(t, P, (b) => b.root('F', e, b.P(1), up(b), [b.plain('L', []), b.plain(s, kidsP(b, s))]))
        break
      case 'stem†':
        if (s === 'S') rule(t, P, (b) => b.head(e, b.P(2), b.P(1), up(b)))
        break
      case 'unit†':
        if (s === 'F')
          rule(t, P, (b) => {
            const k = b.mk(`Ku${e}`)
            b.w(k, 0, b.P(2))
            b.w(k, 1, b.P(1))
            b.w(k, 2, up(b))
            b.w(k, 3, b.P(3))
          })
        break
      case 'cross':
      case 'sink': {
        const k = eff === 'cross' ? (s === 'F' ? 'Kc' : null) : s === 'S' ? 'Kss' : s === 'F' ? 'Ksf' : null
        if (!k) rule(t, P, (b) => b.root(s, e, b.P(1), up(b), kidsP(b, s)))
        else
          rule(t, P, (b) => {
            const r = b.mk(`${k}${e}`)
            b.w(r, 0, b.P(2))
            b.w(r, 1, b.P(1))
            b.w(r, 2, up(b))
            if (s === 'F') b.w(r, 3, b.P(3))
          })
      }
    }
  }
}
// readers: [child, front, back, held]
for (const e of DIRS) {
  const front = (b: B) => b.C(1)
  const back = (b: B) => b.C(2)
  rule(`Ku${e}`, 'L', (b) => b.head(e, b.C(3), front(b), back(b)))
  // cross: ((u, v), w) → ((u, w), v); a non-fork left is left as it is
  rule(`Kc${e}`, 'F', (b) => b.root('F', e, front(b), back(b), [b.plain('F', [b.P(1), b.C(3)]), b.P(2)]))
  rule(`Kc${e}`, 'L', (b) => b.root('F', e, front(b), back(b), [b.plain('L', []), b.C(3)]))
  rule(`Kc${e}`, 'S', (b) => b.root('F', e, front(b), back(b), [b.plain('S', [b.P(1)]), b.C(3)]))
  // sink: △(l, r) ↔ (△l, r)
  rule(`Kss${e}`, 'F', (b) => b.root('F', e, front(b), back(b), [b.plain('S', [b.P(1)]), b.P(2)]))
  rule(`Kss${e}`, 'L', (b) => b.root('S', e, front(b), back(b), [b.plain('L', [])]))
  rule(`Kss${e}`, 'S', (b) => b.root('S', e, front(b), back(b), [b.plain('S', [b.P(1)])]))
  rule(`Ksf${e}`, 'S', (b) => b.root('S', e, front(b), back(b), [b.plain('F', [b.P(1), b.C(3)])]))
  rule(`Ksf${e}`, 'L', (b) => b.root('F', e, front(b), back(b), [b.plain('L', []), b.C(3)]))
  rule(`Ksf${e}`, 'F', (b) => b.root('F', e, front(b), back(b), [b.plain('F', [b.P(1), b.P(2)]), b.C(3)]))
  // a head reads the node it faces, which becomes the top
  for (const s of SHAPES) rule(H(e), s, (b) => b.root(s, e, b.C(1), b.C(2), ends(NKIDS[s], 1, b.P)))
}

bounce('Id')

// a; b: into a (or b, going backward), from one to the other, and out
for (const [s, e] of roots()) {
  const P = R(s, e)
  const fwd = e === 'f'
  // from above: Seq0 [parent, a, b]
  rule('Seq0', P, (b) => {
    const n = b.mk(fwd ? 'SeqA' : 'SeqB')
    b.w(n, 1, b.P(1))
    b.w(n, 2, fwd ? b.C(2) : b.C(1))
    b.root(s, e, fwd ? b.C(1) : b.C(2), b.X(n, 0), kidsP(b, s))
  })
  // out of a: SeqA [a, parent, b]; the root's back leads to a
  rule('SeqA', P, (b) => {
    if (fwd) {
      const n = b.mk('SeqB')
      b.w(n, 1, b.C(1))
      b.w(n, 2, b.P(1))
      b.root(s, e, b.C(2), b.X(n, 0), kidsP(b, s))
    } else {
      const n = b.mk('Seq0')
      b.w(n, 1, b.P(1))
      b.w(n, 2, b.C(2))
      b.root(s, e, b.C(1), b.X(n, 0), kidsP(b, s))
    }
  })
  // out of b: SeqB [b, parent, a]
  rule('SeqB', P, (b) => {
    if (fwd) {
      const n = b.mk('Seq0')
      b.w(n, 1, b.C(2))
      b.w(n, 2, b.P(1))
      b.root(s, e, b.C(1), b.X(n, 0), kidsP(b, s))
    } else {
      const n = b.mk('SeqA')
      b.w(n, 1, b.C(1))
      b.w(n, 2, b.P(1))
      b.root(s, e, b.C(2), b.X(n, 0), kidsP(b, s))
    }
  })
}

// tri on one shape at the value's left: [parent, part] at rest, [part, parent] running
for (const t of Object.keys(TRIS) as Tri[]) {
  const want: Shape = ({ TL: 'L', TS: 'S', TF: 'F' } as const)[t]
  for (const [s, e] of roots()) {
    const P = R(s, e)
    if (s !== 'F') {
      rule(`${t}0`, P, (b) => {
        const n = b.mk(`${t}0`)
        b.w(n, 1, b.C(1))
        b.root(s, e, b.P(1), b.X(n, 0), kidsP(b, s))
      })
      continue
    }
    // a fork: the tri takes it in and reads its left [s, parent, part, rest]
    rule(`${t}0`, P, (b) => {
      const r = b.mk(`${t}r${e}`)
      b.w(r, 0, b.P(2))
      b.w(r, 1, b.P(1))
      b.w(r, 2, b.C(1))
      b.w(r, 3, b.P(3))
    })
  }
  // reading the left: [s, parent, part, rest]
  for (const e of DIRS)
    for (const turned of [false, true]) {
      const me = `${t}r${e}${turned ? 'p' : ''}`
      for (const v of SHAPES) {
        if (v !== want) {
          // not this part's shape: the value goes back up as it came
          rule(me, v, (b) => {
            const n = b.mk(`${t}0`)
            b.w(n, 1, b.C(2))
            b.root('F', e, b.C(1), b.X(n, 0), [b.plain(v, ends(NKIDS[v], 1, b.P)), b.C(3)], turned)
          })
          continue
        }
        rule(me, v, (b) => {
          const n = b.mk(`${t}i`)
          b.w(n, 1, b.C(1))
          const at = b.X(n, 0)
          if (v === 'L') b.head(e, b.C(3), b.C(2), at, turned)
          // (△t, rest) runs the part on (t, rest)
          else if (v === 'S') b.root('F', e, b.C(2), at, [b.P(1), b.C(3)], turned)
          // ((s1, s2), rest) runs it on (s1, (s2, rest))
          else b.root('F', e, b.C(2), at, [b.P(1), b.plain('F', [b.P(2), b.C(3)])], turned)
        })
      }
    }
  // the run coming out of the part: [part, parent]; the root's back leads into the part
  for (const [s, e] of roots()) {
    const P = R(s, e)
    if (t === 'TL')
      rule('TLi', P, (b) => {
        const n = b.mk('TL0')
        b.w(n, 1, b.P(1))
        b.root('F', e, b.C(1), b.X(n, 0), [b.plain('L', []), b.plain(s, kidsP(b, s))])
      })
    else if (s === 'F' && t === 'TS')
      rule('TSi', P, (b) => {
        const n = b.mk('TS0')
        b.w(n, 1, b.P(1))
        b.root('F', e, b.C(1), b.X(n, 0), [b.plain('S', [b.P(2)]), b.P(3)])
      })
    else if (s === 'F' && t === 'TF')
      // (o1, x): x is read for (o2, o3), to put back ((o1, o2), o3) [x, parent, part, o1]
      rule('TFi', P, (b) => {
        const r = b.mk(`TFc${e}`)
        b.w(r, 0, b.P(3))
        b.w(r, 1, b.C(1))
        b.w(r, 2, b.P(1))
        b.w(r, 3, b.P(2))
      })
  }
}
for (const e of DIRS)
  for (const turned of [false, true])
    rule(`TFc${e}${turned ? 'p' : ''}`, 'F', (b) => {
      const n = b.mk('TF0')
      b.w(n, 1, b.C(2))
      b.root('F', e, b.C(1), b.X(n, 0), [b.plain('F', [b.C(3), b.P(1)]), b.P(2)], turned)
    })

// map on a stem: [parent, a] at rest, [a, parent] running
for (const [s, e] of roots()) {
  const P = R(s, e)
  if (s === 'S')
    rule('M0', P, (b) => {
      const n = b.mk('Mi')
      b.w(n, 1, b.P(1))
      b.head(e, b.P(2), b.C(1), b.X(n, 0))
    })
  else
    rule('M0', P, (b) => {
      const n = b.mk('M0')
      b.w(n, 1, b.C(1))
      b.root(s, e, b.P(1), b.X(n, 0), kidsP(b, s))
    })
  rule('Mi', P, (b) => {
    const n = b.mk('M0')
    b.w(n, 1, b.P(1))
    b.root('S', e, b.C(1), b.X(n, 0), [b.plain(s, kidsP(b, s))])
  })
}

// map on a fork's halves at once: P0 [parent, b, c]; PB [b, parent, c]; PHC [c, parent, b, held left]
for (const [s, e] of roots()) {
  const P = R(s, e)
  if (s === 'F')
    rule('P0', P, (b) => {
      const n = b.mk('PB')
      b.w(n, 1, b.P(1))
      b.head(e, b.P(2), b.C(1), b.X(n, 0))
      b.head(e, b.P(3), b.C(2), b.X(n, 2))
    })
  else
    rule('P0', P, (b) => {
      const n = b.mk('P0')
      b.w(n, 1, b.C(1))
      b.w(n, 2, b.C(2))
      b.root(s, e, b.P(1), b.X(n, 0), kidsP(b, s))
    })
  // the left half back: held, and the map waits for the right
  rule('PB', P, (b) => {
    const n = b.mk(`PHC${e}`)
    b.w(n, 0, b.C(2))
    b.w(n, 1, b.C(1))
    b.w(n, 2, b.P(1))
    b.w(n, 3, b.plain(s, kidsP(b, s)))
  })
  // the right half back: the fork is whole again, and goes up
  for (const h of DIRS)
    rule(`PHC${h}`, P, (b) => {
      const n = b.mk('P0')
      b.w(n, 1, b.C(2))
      b.w(n, 2, b.P(1))
      b.root('F', e, b.C(1), b.X(n, 0), [b.C(3), b.plain(s, kidsP(b, s))])
    })
}
// turned round holding the left half [held, parent, b, c]: the half goes back down into b
for (const e of DIRS)
  for (const v of SHAPES)
    rule(`PHR${e}`, v, (b) => {
      const n = b.mk('PB')
      b.w(n, 1, b.C(1))
      b.w(n, 2, b.C(3))
      b.root(v, e, b.C(2), b.X(n, 0), ends(NKIDS[v], 1, b.P))
    })

// map on a fork's left half only: Fst0 [parent, b]; FstB [b, parent, held right]; and the right half only
for (const [s, e] of roots()) {
  const P = R(s, e)
  for (const [rest, run, keep, kidRun, kidKeep] of [
    ['Fst0', 'FstB', 3, 2, 3],
    ['Snd0', 'SndC', 3, 3, 2]
  ] as const) {
    if (s === 'F')
      rule(rest, P, (b) => {
        const n = b.mk(run)
        b.w(n, 1, b.P(1))
        b.w(n, 2, b.P(kidKeep))
        b.head(e, b.P(kidRun), b.C(1), b.X(n, 0))
      })
    else
      rule(rest, P, (b) => {
        const n = b.mk(rest)
        b.w(n, 1, b.C(1))
        b.root(s, e, b.P(1), b.X(n, 0), kidsP(b, s))
      })
    void keep
    rule(run, P, (b) => {
      const n = b.mk(rest)
      b.w(n, 1, b.P(1))
      const half = b.plain(s, kidsP(b, s))
      b.root('F', e, b.C(1), b.X(n, 0), run === 'FstB' ? [half, b.C(2)] : [b.C(2), half])
    })
  }
}

/** The rule for a meeting pair, the consumer first. */
const INDEX = new Map(RULES.map((r) => [`${r.consumer}·${r.producer}`, r]))
export const ruleFor = (consumer: string, producer: string) => INDEX.get(`${consumer}·${producer}`)
