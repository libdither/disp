// What each part of a function's finished tree does to which input: run it on
// one symbol per parameter, and follow every node of the run back to the node
// of the finished tree it came from. A fork that fires S on an input copies it,
// one that fires K drops it, one that fires triage looks at its shape; an S
// whose one side drops the input at once just sends it the other way. I hands
// an input on unchanged, so that is where it lands, as is a leaf that grows into
// the stem or fork an input ends up hanging from.

import { Store, APP, FORK, STEM, VAR, ATOM, HELD_K, R_K, R_S, R_F, type Id } from './store.ts'
import { Trace, type Mode } from './trace.ts'

export type RoleKind = 'copy' | 'left' | 'right' | 'drop' | 'look'

export interface RoleMark {
  /// the drawing key of the deciding fork in the finished tree
  key: string
  kind: RoleKind
  input: string
  /// the Try it frame where it happens, the step about to fire
  frame: number
  /// it happens in the step about to fire on the frame showing
  now?: boolean
}

/// a role mark under the pointer, or the inputs a leaf ends up holding
export type RoleHit = { mark: RoleMark; hold?: undefined } | { mark?: undefined; hold: { key: string; inputs: string[] } }

export interface Roles {
  marks: RoleMark[]
  /// where inputs land, by key: an I they pass through, or a leaf that ends up holding them in the result
  holds: Map<string, string[]>
  /// the input each branch handles, by the key of the node the branch leads to
  tint: Map<string, string>
  inputs: string[]
  /// the run went past the limit, so some inputs may have more to do
  partial: boolean
  /// the input the step about to fire acts on, whose branches are shaded
  active?: string | null
}

/// a node of the run beside where it came from in the finished tree (null: built by the run)
interface Origin {
  o: string | null
  k: (Origin | null)[]
}

const MAX_FRAMES = 20_000
const kidO = (o: Origin | null, i: number): Origin | null => o?.k[i] ?? null
const made = (...k: (Origin | null)[]): Origin => ({ o: null, k })

/// what the drawing shows in place of an id: a named atom or a resolved cut is its tree
function resolve(s: Store, id: Id): Id {
  let cur = id
  for (let r = s.resolved.get(cur); r !== undefined; r = s.resolved.get(cur)) cur = r
  return s.tag[cur] === ATOM ? s.a[cur] : cur
}

/// The newest input inside a term: earlier ones are carried along, the newest is the one being routed.
export function newestInput(s: Store, id: Id, inputs: string[]): string | null {
  let best = -1
  const stack = [id]
  for (let n = 0; stack.length && n < 4000; n++) {
    const c = resolve(s, stack.pop()!)
    const t = s.tag[c]
    if (t === VAR) best = Math.max(best, inputs.indexOf(s.strs[s.a[c]]))
    else if (t === STEM || t === HELD_K) stack.push(s.a[c])
    else if (t === FORK || t === APP) stack.push(s.a[c], s.b[c])
  }
  return best >= 0 ? inputs[best] : null
}

export class RoleRun {
  readonly trace: Trace
  #done = 0
  #at: Origin
  /// each frame's origins, so the roles can be drawn on any frame of the run
  #history: Origin[] = []
  #marks: RoleMark[] = []
  #seen = new Set<string>()
  /// I's an input went through, in order
  #lands: [string, string][] = []

  constructor(
    readonly store: Store,
    readonly program: Id,
    readonly inputs: string[],
    mode: Mode = 'eager'
  ) {
    let root = program
    let at = this.#origins(program, 'r0')
    for (const p of inputs) {
      root = store.app(root, store.variable(p))
      at = made(at, null)
    }
    this.#at = at
    this.#history.push(at)
    this.trace = new Trace(store, root, mode)
  }

  #res = (id: Id): Id => resolve(this.store, id)

  #origins(id: Id, key: string): Origin {
    const s = this.store
    const t = s.tag[(id = this.#res(id))]
    if (t === STEM) return { o: key, k: [this.#origins(s.a[id], key + '0')] }
    if (t === HELD_K) return { o: key, k: [{ o: key + '0', k: [] }, this.#origins(s.a[id], key + '1')] }
    if (t === FORK) return { o: key, k: [this.#origins(s.a[id], key + '0'), this.#origins(s.b[id], key + '1')] }
    return { o: key, k: [] }
  }

  #newest = (id: Id) => newestInput(this.store, id, this.inputs)

  #mark(key: string | null, kind: RoleKind, x: Id, frame: number) {
    const input = key && this.#newest(x)
    if (!key || !input || this.#seen.has(key + kind + input)) return
    this.#seen.add(key + kind + input)
    this.#marks.push({ key, kind, input, frame })
  }

  /// Run some more; true while there is more to do.
  advance(ms: number): boolean {
    const tr = this.trace
    const more = tr.advance(ms) && tr.last < MAX_FRAMES
    for (; this.#done < tr.last; this.#done++) {
      const root = tr.frames[this.#done]
      // a parallel step fires sites side by side, none inside another, so each rewrites its own part of the origins
      const paths = tr.mode === 'parallel' ? tr.readyPaths(root, Infinity) : [tr.nextPath(root)]
      if (!paths[0]) break
      for (const path of paths) this.#at = this.#fire(root, path!)
      this.#history.push(this.#at)
    }
    return more
  }

  /// the origins after the application at `path` in `root` fires
  #fire(root: Id, path: number[]): Origin {
    const s = this.store
    const spine: (Origin | null)[] = []
    let o: Origin | null = this.#at
    for (const i of path) {
      spine.push(o)
      o = kidO(o, i)
    }
    const site = this.trace.subterm(root, path)
    const of = kidO(o, 0)
    const ox = kidO(o, 1)
    const x = s.b[site]
    let out: Origin | null
    switch (s.rule[site]) {
      case R_K:
        this.#mark(of?.o ?? null, 'drop', x, this.#done)
        out = kidO(of, 1)
        break
      case R_S:
        this.#mark(of?.o ?? null, 'copy', x, this.#done)
        out = made(made(kidO(kidO(of, 0), 0), ox), made(kidO(of, 1), ox))
        break
      case R_F: {
        const input = this.#newest(x)
        if (this.#res(s.a[site]) === s.i) {
          if (of?.o && input) this.#lands.push([of.o, input])
        } else this.#mark(of?.o ?? null, 'look', x, this.#done)
        const t = s.tag[this.#res(x)]
        const tri = kidO(of, 0)
        out = t === FORK ? made(made(kidO(of, 1), kidO(ox, 0)), kidO(ox, 1)) : t === STEM ? made(kidO(tri, 1), kidO(ox, 0)) : t === VAR || t === APP ? ox : kidO(tri, 0)
        break
      }
      default:
        // a leaf grows a stem round the argument, a stem a fork: the new node is still that part of the tree
        out = { o: of?.o ?? null, k: s.tag[this.#res(s.a[site])] === STEM || s.tag[this.#res(s.a[site])] === HELD_K ? [kidO(of, 0), ox] : [ox] }
    }
    for (let k = path.length - 1; k >= 0; k--) {
      const p = spine[k]
      const kids = p ? [...p.k] : [null, null]
      kids[path[k]] = out
      out = { o: p?.o ?? null, k: kids }
    }
    return out ?? made()
  }

  /// The roles as drawn on frame `f` of the run: keyed by where each part of the finished tree is in that frame,
  /// with only the marks still to happen.
  at(roles: Roles, f: number): Roles | null {
    const top = this.#history[f]
    if (!top) return null
    const where = new Map<string, string[]>()
    const walk = (o: Origin | null, key: string, depth: number) => {
      if (!o || depth > 600) return
      if (o.o) where.set(o.o, [...(where.get(o.o) ?? []), key])
      o.k.forEach((c, i) => walk(c, key + i, depth + 1))
    }
    walk(top, 'r0', 0)
    const marks = roles.marks.filter((m) => m.frame >= f).flatMap((m) => (where.get(m.key) ?? []).map((key) => ({ ...m, key, now: m.frame === f })))
    const holds = new Map<string, string[]>()
    for (const [o, ins] of roles.holds) for (const key of where.get(o) ?? []) holds.set(key, ins)
    const tint = new Map<string, string>()
    for (const [o, input] of roles.tint) for (const key of where.get(o) ?? []) tint.set(key, input)
    return { ...roles, marks, holds, tint }
  }

  get roles(): Roles {
    const s = this.store
    const holds = new Map<string, string[]>()
    const hold = (key: string, name: string) => {
      const at = holds.get(key) ?? []
      if (!at.includes(name)) holds.set(key, [...at, name])
    }
    for (const [key, name] of this.#lands) hold(key, name)
    // an S that copies an input into a side that drops it at once only sends it the other way
    const found = (key: string, kind: RoleKind, input: string) => this.#marks.find((m) => m.key === key && m.kind === kind && m.input === input)
    const gone = new Set<RoleMark>()
    const marks: RoleMark[] = []
    for (const m of this.#marks) {
      if (gone.has(m)) continue
      if (m.kind !== 'copy') {
        marks.push(m)
        continue
      }
      const left = found(m.key + '00', 'drop', m.input)
      const right = found(m.key + '1', 'drop', m.input)
      if (left) gone.add(left)
      if (right) gone.add(right)
      marks.push({ ...m, kind: left && right ? 'drop' : left ? 'right' : right ? 'left' : 'copy' })
    }
    for (let i = marks.length - 1; i >= 0; i--) if (gone.has(marks[i])) marks.splice(i, 1)
    const end = this.trace.frames[this.#done]
    // where each input ends up: the part of the tree it hangs from; an input stuck before a triage is looked at
    const walk = (id: Id, o: Origin | null, parent: Origin | null, depth: number) => {
      if (depth > 400) return
      const c = this.#res(id)
      const t = s.tag[c]
      if (t === VAR) {
        const name = s.strs[s.a[c]]
        if (parent?.o && this.inputs.includes(name)) hold(parent.o, name)
        return
      }
      if (t === APP) {
        const f = this.#res(s.a[c])
        const fo = kidO(o, 0)
        if (fo?.o && f !== s.i && s.tag[f] === FORK && s.tag[this.#res(s.a[f])] === FORK) {
          const input = this.#newest(s.b[c])
          if (input && !this.#seen.has(fo.o + 'look' + input)) marks.push({ key: fo.o, kind: 'look', input, frame: this.#done })
        }
      }
      if (t === STEM || t === HELD_K) walk(s.a[c], kidO(o, t === HELD_K ? 1 : 0), o, depth + 1)
      else if (t === FORK || t === APP) {
        walk(s.a[c], kidO(o, 0), o, depth + 1)
        walk(s.b[c], kidO(o, 1), o, depth + 1)
      }
    }
    walk(end, this.#at, null, 0)
    // a branch takes the colour of the first input handled at its end: the deciding fork and the side that decides, or the leaf that holds it
    const tint = new Map<string, string>()
    const paint = (key: string, input: string) => {
      if (!tint.has(key)) tint.set(key, input)
    }
    for (const m of [...marks].sort((a, b) => a.frame - b.frame)) {
      paint(m.key, m.input)
      if (m.kind === 'right') paint(m.key + '1', m.input)
      else paint(m.key + '0', m.input)
    }
    for (const [key, ins] of holds) paint(key, ins[0])
    return { marks, holds, tint, inputs: this.inputs, partial: this.trace.end === null }
  }
}
