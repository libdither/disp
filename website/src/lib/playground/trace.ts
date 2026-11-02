// A reduction trace: every frame of a term's run, computed ahead of the viewer
// so the timeline can scrub anywhere. Innermost-first, arguments before the
// application, the same order the real eager evaluator uses, so the calls it
// finds are the calls the program makes. Frames share structure through the
// store, so a long trace costs one id per frame.

import { Store, APP, FORK, STEM, LEAF, VAR, R_LEAF, R_STEM, R_K, R_S, R_F, R_NONE, type Id } from './store.ts'

export interface Call {
  /// the frame whose rule firing starts this call
  start: number
  /// first frame where the call has finished; -1 while it runs
  end: number
  name: string
  args: Id[]
  result: Id
  depth: number
  path: number[]
}

export type Mode = 'eager' | 'parallel'
export type TraceEnd = 'normal' | 'stuck' | 'limit' | null

export const MAX_FRAMES = 200_000
const MAX_NODES = 6_000_000

export class Trace {
  /// frames[i] is the term after i firings; rules[i] is the rule that produced it (rules[0] = 0)
  frames: Id[]
  rules: number[] = [R_NONE]
  /// how many redexes fired to reach this frame (parallel mode fires many)
  fired: number[] = [0]
  sizes: number[]
  calls: Call[] = []
  end: TraceEnd = null
  #stack: Call[] = []

  constructor(
    readonly store: Store,
    root: Id,
    readonly mode: Mode = 'eager'
  ) {
    this.frames = [root]
    this.sizes = [store.size[root]]
    if (!store.hasRedex(root)) this.end = store.hasApp(root) ? 'stuck' : 'normal'
  }

  get last(): number {
    return this.frames.length - 1
  }

  /// The next redex under `from` in evaluation order, as a child-index path; null when there is none.
  nextPath(from: Id): number[] | null {
    const s = this.store
    if (!s.hasRedex(from)) return null
    const path: number[] = []
    let id = from
    for (;;) {
      const t = s.tag[id]
      if (t === STEM) {
        path.push(0)
        id = s.a[id]
      } else {
        const a = s.a[id]
        if (s.hasRedex(a)) {
          path.push(0)
          id = a
        } else if (s.hasRedex(s.b[id])) {
          path.push(1)
          id = s.b[id]
        } else return path
      }
    }
  }

  /// One rule firing at the application `site`.
  fireNode(site: Id): Id {
    const s = this.store
    const f = s.a[site]
    const x = s.b[site]
    switch (s.rule[site]) {
      case R_LEAF:
        return s.stem(x)
      case R_STEM:
        return s.fork(s.a[f], x)
      case R_K:
        return s.b[f]
      case R_S:
        return s.app(s.app(s.a[s.a[f]], x), s.app(s.b[f], x))
      case R_F: {
        const tri = s.a[f]
        const xt = s.tag[x]
        if (xt === VAR || xt === APP) return x
        if (xt === LEAF) return s.a[tri]
        if (xt === STEM) return s.app(s.b[tri], s.a[x])
        return s.app(s.app(s.b[f], s.a[x]), s.b[x])
      }
    }
    return site
  }

  #replaceAt(root: Id, path: number[], fresh: Id): Id {
    const s = this.store
    const spine: Id[] = []
    let id = root
    for (const i of path) {
      spine.push(id)
      id = s.tag[id] === STEM || i === 0 ? s.a[id] : s.b[id]
    }
    let cur = fresh
    for (let k = path.length - 1; k >= 0; k--) {
      const p = spine[k]
      const t = s.tag[p]
      if (t === STEM) cur = s.stem(cur)
      else if (t === FORK) cur = path[k] === 0 ? s.fork(cur, s.b[p]) : s.fork(s.a[p], cur)
      else cur = path[k] === 0 ? s.app(cur, s.b[p]) : s.app(s.a[p], cur)
    }
    return cur
  }

  subterm(root: Id, path: number[]): Id {
    const s = this.store
    let id = root
    for (const i of path) id = s.tag[id] === STEM || i === 0 ? s.a[id] : s.b[id]
    return id
  }

  /// Record a call when the firing application's head is a name in scope.
  #noteCall(root: Id, path: number[], frame: number) {
    const s = this.store
    const site = this.subterm(root, path)
    const name = s.names.get(s.a[site])
    if (!name) return
    // climb the application spine: `add 2 3` fires `add 2` first
    const args = [s.b[site]]
    const ancestors: Id[] = []
    let id = root
    for (const i of path) {
      ancestors.push(id)
      id = s.tag[id] === STEM || i === 0 ? s.a[id] : s.b[id]
    }
    let top = path.length
    while (top > 0 && path[top - 1] === 0 && s.tag[ancestors[top - 1]] === APP) {
      top--
      args.push(s.b[ancestors[top]])
    }
    const call: Call = { start: frame, end: -1, name, args, result: -1, depth: this.#stack.length, path: path.slice(0, top) }
    this.calls.push(call)
    this.#stack.push(call)
  }

  #closeCalls(root: Id, frame: number) {
    const s = this.store
    while (this.#stack.length > 0) {
      const top = this.#stack[this.#stack.length - 1]
      const sub = this.subterm(root, top.path)
      if (s.hasRedex(sub)) break
      top.end = frame
      top.result = sub
      this.#stack.pop()
    }
  }

  #push(root: Id, rule: number, fired: number) {
    this.frames.push(root)
    this.rules.push(rule)
    this.fired.push(this.fired[this.fired.length - 1] + fired)
    this.sizes.push(this.store.size[root])
  }

  /// One frame forward; false once the trace has ended.
  step(): boolean {
    if (this.end) return false
    const s = this.store
    const root = this.frames[this.last]
    if (this.last >= MAX_FRAMES || s.n >= MAX_NODES) {
      this.end = 'limit'
      return false
    }
    let next: Id
    if (this.mode === 'parallel') {
      const out = this.#parallel(root)
      next = out.id
      this.#push(next, out.rule, out.fired)
    } else {
      const path = this.nextPath(root)!
      this.#noteCall(root, path, this.last)
      const site = this.subterm(root, path)
      next = this.#replaceAt(root, path, this.fireNode(site))
      this.#push(next, s.rule[site], 1)
      this.#closeCalls(next, this.last)
    }
    if (!s.hasRedex(next)) {
      this.end = s.hasApp(next) ? 'stuck' : 'normal'
      return false
    }
    return true
  }

  // Fire every innermost-ready redex in one pass: an application fires only
  // once nothing below it can.
  #parallel(root: Id): { id: Id; rule: number; fired: number } {
    const s = this.store
    let fired = 0
    let rule = R_NONE
    const memo = new Map<Id, Id>()
    const go = (id: Id): Id => {
      if (!s.hasRedex(id)) return id
      const hit = memo.get(id)
      if (hit !== undefined) {
        return hit
      }
      const t = s.tag[id]
      let out: Id
      if (t === STEM) out = s.stem(go(s.a[id]))
      else if (t === FORK) out = s.fork(go(s.a[id]), go(s.b[id]))
      else if (s.hasRedex(s.a[id]) || s.hasRedex(s.b[id])) out = s.app(go(s.a[id]), go(s.b[id]))
      else {
        fired++
        rule = s.rule[id]
        out = this.fireNode(id)
      }
      memo.set(id, out)
      return out
    }
    const id = go(root)
    return { id, rule, fired }
  }

  /// Compute frames for at most `ms`; true while more remain.
  advance(ms: number): boolean {
    const t0 = performance.now()
    let n = 0
    while (this.step()) {
      if ((++n & 63) === 0 && performance.now() - t0 >= ms) return true
    }
    return false
  }

  /// Fire the application at `path` in frame `frame`, reducing inside it first if it isn't ready, dropping any later frames. Returns the frame reached.
  fireAt(frame: number, path: number[]): number {
    this.frames.length = frame + 1
    this.rules.length = frame + 1
    this.fired.length = frame + 1
    this.sizes.length = frame + 1
    this.calls = this.calls.filter((c) => c.start < frame)
    this.#stack = []
    for (const c of this.calls)
      if (c.end > frame || c.end < 0) {
        c.end = -1
        this.#stack.push(c)
      }
    this.end = null
    const s = this.store
    for (let i = 0; i < 4000; i++) {
      const root = this.frames[this.last]
      const site = this.subterm(root, path)
      if (s.tag[site] !== APP) break
      const inner = this.nextPath(site)
      if (!inner) break
      const full = [...path, ...inner]
      const target = this.subterm(root, full)
      const rule = s.rule[target]
      const next = this.#replaceAt(root, full, this.fireNode(target))
      this.#push(next, rule, 1)
      if (inner.length === 0) break
    }
    const cur = this.frames[this.last]
    if (!s.hasRedex(cur)) this.end = s.hasApp(cur) ? 'stuck' : 'normal'
    return this.last
  }

  /// The call a frame sits inside, innermost.
  callAt(frame: number): Call | null {
    let best: Call | null = null
    for (const c of this.calls) {
      if (c.start > frame) break
      if (c.end < 0 || c.end > frame) best = c
    }
    return best
  }
}
