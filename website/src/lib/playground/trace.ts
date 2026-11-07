// A reduction trace: every frame of a term's run, computed ahead of the viewer
// so the timeline can scrub anywhere. Innermost-first, arguments before the
// application, the same order the real eager evaluator uses, so the calls it
// finds are the calls the program makes. Frames share structure through the
// store, so a long trace costs one id per frame.
//
// A binder coming off is one more kind of step, and by default every binder
// comes off before anything runs. Nothing runs inside a binder until it is gone
// (that keeps the result the compiler's), and an application waits until what it
// looks at has no binders; anything else may be clicked to run early, and the
// end is still the same tree as removing every binder first.

import { Store, APP, FORK, LEAF, STEM, VAR, LAM, SPLIT, HELD_K, R_LEAF, R_STEM, R_K, R_S, R_F, R_NONE, R_BIND, type Id } from './store.ts'

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
  /// how many of those were binder rewrites
  bound: number[] = [0]
  sizes: number[]
  calls: Call[] = []
  end: TraceEnd = null
  #stack: Call[] = []

  constructor(
    readonly store: Store,
    root: Id,
    public mode: Mode = 'eager'
  ) {
    this.frames = [root]
    this.sizes = [store.size[root]]
    if (!store.hasRedex(root)) this.end = store.hasApp(root) ? 'stuck' : 'normal'
  }

  get last(): number {
    return this.frames.length - 1
  }

  /// The next redex under `from` in evaluation order, as a child-index path; null when there is none.
  /// While a binder is left to come off, that comes first.
  nextPath(from: Id): number[] | null {
    const s = this.store
    if (!s.hasRedex(from)) return null
    const path: number[] = []
    let id = from
    let inside = s.bindReady(from)
    for (;;) {
      const t = s.tag[id]
      if (t === LAM || t === SPLIT) {
        if (s.rule[id] === R_BIND) return path
        inside = true
      }
      const live = inside ? s.bindReady : s.hasRedex
      let went = false
      for (const i of childIndexes(t)) {
        const c = s.kid(id, i)
        if (live(c)) {
          path.push(i)
          id = c
          went = true
          break
        }
      }
      if (!went) return path
    }
  }

  /// Every redex a parallel step fires, as paths (at most `cap`).
  readyPaths(from: Id, cap = 300): number[][] {
    const s = this.store
    const out: number[][] = []
    const go = (id: Id, path: number[], inside: boolean) => {
      if (out.length >= cap || !(inside ? s.bindReady(id) : s.hasRedex(id))) return
      const t = s.tag[id]
      if (t === LAM || t === SPLIT) {
        if (s.rule[id] === R_BIND) return void out.push(path)
        inside = true
      }
      const ks = childIndexes(t)
      if (t === APP && !inside && !ks.some((i) => s.hasRedex(s.kid(id, i)))) return void out.push(path)
      for (const i of ks) go(s.kid(id, i), [...path, i], inside)
    }
    go(from, [], s.bindReady(from))
    return out
  }

  /// One rule firing at the application `site`, or one binder rewrite.
  fireNode(site: Id): Id {
    const s = this.store
    if (s.rule[site] === R_BIND) return s.bindStep(site)
    const f = s.a[site]
    const x = s.b[site]
    switch (s.rule[site]) {
      case R_LEAF:
        return s.stem(x)
      case R_STEM:
        return s.fork(s.ta(f), x)
      case R_K:
        return s.tb(f)
      case R_S:
        return s.app(s.app(s.ta(s.ta(f)), x), s.app(s.tb(f), x))
      case R_F: {
        const tri = s.ta(f)
        const xt = s.tt(x)
        if (xt === VAR || xt === APP) return x
        if (xt === LEAF) return s.ta(tri)
        if (xt === STEM) return s.app(s.tb(tri), s.ta(x))
        return s.app(s.app(s.tb(f), s.ta(x)), s.tb(x))
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
      id = s.kid(id, i)
    }
    let cur = fresh
    for (let k = path.length - 1; k >= 0; k--) cur = s.withKid(spine[k], path[k], cur)
    return cur
  }

  subterm(root: Id, path: number[]): Id {
    let id = root
    for (const i of path) id = this.store.kid(id, i)
    return id
  }

  /// Record a call when the firing application's head is a name in scope.
  #noteCall(root: Id, path: number[], frame: number) {
    const s = this.store
    const site = this.subterm(root, path)
    if (s.tag[site] !== APP) return
    const name = s.names.get(s.a[site])
    if (!name) return
    // climb the application spine: `add 2 3` fires `add 2` first
    const args = [s.b[site]]
    const ancestors: Id[] = []
    let id = root
    for (const i of path) {
      ancestors.push(id)
      id = s.kid(id, i)
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

  #push(root: Id, rule: number, fired: number, bound: number) {
    this.frames.push(root)
    this.rules.push(rule)
    this.fired.push(this.fired[this.fired.length - 1] + fired)
    this.bound.push(this.bound[this.bound.length - 1] + bound)
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
      this.#push(next, out.rule, out.fired, out.bound)
    } else {
      const path = this.nextPath(root)!
      this.#noteCall(root, path, this.last)
      const site = this.subterm(root, path)
      next = this.#replaceAt(root, path, this.fireNode(site))
      this.#push(next, s.rule[site], 1, s.rule[site] === R_BIND ? 1 : 0)
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
  #parallel(root: Id): { id: Id; rule: number; fired: number; bound: number } {
    const s = this.store
    let fired = 0
    let bound = 0
    let rule = R_NONE
    const memos = [new Map<Id, Id>(), new Map<Id, Id>()]
    // inside an unfinished binder only binder rewrites happen
    const go = (id: Id, inside: number): Id => {
      if (!(inside ? s.bindReady(id) : s.hasRedex(id))) return id
      const hit = memos[inside].get(id)
      if (hit !== undefined) return hit
      const t = s.tag[id]
      let out: Id
      if (s.rule[id] === R_BIND || (t === APP && !inside && !s.hasRedex(s.a[id]) && !s.hasRedex(s.b[id]))) {
        fired++
        if (s.rule[id] === R_BIND) bound++
        // the frame is labelled by a real rule when one fired, so it isn't skipped as only building data
        if (rule <= R_STEM) rule = s.rule[id]
        out = this.fireNode(id)
      } else if (t === LAM) out = s.withKid(id, 0, go(s.b[id], 1))
      else if (t === SPLIT) out = s.split(go(s.a[id], 1), go(s.b[id], 1))
      else if (t === STEM) out = s.stem(go(s.a[id], inside))
      else if (t === HELD_K) out = s.heldK(go(s.a[id], inside))
      else if (t === APP) out = s.app(go(s.a[id], inside), go(s.b[id], inside))
      else out = s.fork(go(s.a[id], inside), go(s.b[id], inside))
      memos[inside].set(id, out)
      return out
    }
    const id = go(root, s.bindReady(root) ? 1 : 0)
    return { id, rule, fired, bound }
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

  #cut(frame: number) {
    this.frames.length = frame + 1
    this.rules.length = frame + 1
    this.fired.length = frame + 1
    this.bound.length = frame + 1
    this.sizes.length = frame + 1
    this.calls = this.calls.filter((c) => c.start < frame)
    this.#stack = []
    for (const c of this.calls)
      if (c.end > frame || c.end < 0) {
        c.end = -1
        this.#stack.push(c)
      }
    this.end = null
  }

  /// Fire the application or binder at `path` in frame `frame`, reducing inside it first if it isn't ready, dropping any later frames. Returns the frame reached.
  /// An application inside a binder still in place can't fire before that binder is gone, so nothing happens.
  fireAt(frame: number, path: number[]): number {
    const s = this.store
    let id = this.frames[frame]
    let held = false
    for (const i of path) {
      if (s.tag[id] === LAM || s.tag[id] === SPLIT) held = true
      id = s.kid(id, i)
    }
    if (held && s.tag[id] === APP) return frame
    this.#cut(frame)
    for (let i = 0; i < 4000; i++) {
      const root = this.frames[this.last]
      const site = this.subterm(root, path)
      if (s.tag[site] !== APP && s.tag[site] !== LAM && s.tag[site] !== SPLIT) break
      // a ready application fires as clicked, whatever is ready inside it
      const inner = s.tag[site] === APP && s.rule[site] ? [] : this.nextPath(site)
      if (!inner) break
      const full = [...path, ...inner]
      const target = this.subterm(root, full)
      const rule = s.rule[target]
      const next = this.#replaceAt(root, full, this.fireNode(target))
      this.#push(next, rule, 1, rule === R_BIND ? 1 : 0)
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

/// Which children a node of this tag has, in evaluation order (paths as the drawing keys them).
export function childIndexes(t: number): number[] {
  if (t === STEM || t === LAM) return [0]
  if (t === HELD_K) return [1]
  if (t === APP || t === SPLIT || t === FORK) return [0, 1]
  return []
}
