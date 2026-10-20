// A run as a tree that grows and shrinks, one call at a time. The state is a single term: values,
// pending calls (a program applied to an argument), matches waiting on calls, and finished steps.
// Firing a pending call replaces it by its clause's other side, whose own calls are new pending
// calls; those are independent, so the ones ready at the same time can all fire at once: a parallel
// step. A call pattern on the side being matched (what a clause run backward has) becomes a pending
// call the other way, and a match node waits for it before building. Every step can be taken back: a
// finished step keeps what it replaced, and undoing a call is checked against running it backward.

import {
  type V,
  L as VL,
  S as VS,
  F as VF,
  progShape,
  patShape,
  progName,
  clauseNames,
  nameFor,
  eq,
  key,
  run,
} from './patterns.ts'

let nextId = 2_000_000_000
const fresh = () => nextId++

type Env = Map<string, { n: V; t: Term }>

export type Term =
  | { k: 'leaf'; id: number }
  | { k: 'stem'; c: Term; id: number }
  | { k: 'fork'; l: Term; r: Term; id: number }
  /** A pending call: prog run on arg (backward if back). pod is the id its folded program is drawn with. */
  | { k: 'call'; prog: V; back: boolean; arg: Term; id: number; pod: number; name: string }
  /** A match waiting on calls: once each bind's term is a value, match it against its pattern, then
   * build out. */
  | { k: 'let'; binds: { p: V; t: Term }[]; env: Env; out: V; me: V; meName: string; names?: string[]; rule: string; id: number }
  /** A finished step: body is what it gave, was the pending call or match it replaced, t when. */
  | { k: 'done'; body: Term; was: Call | Let; rule: string; t: number; id: number }

export type Call = Term & { k: 'call' }
export type Let = Term & { k: 'let' }
export type Done = Term & { k: 'done' }

const leaf = (): Term => ({ k: 'leaf', id: fresh() })
const stem = (c: Term): Term => ({ k: 'stem', c, id: fresh() })
const fork = (l: Term, r: Term): Term => ({ k: 'fork', l, r, id: fresh() })
export const mkCall = (prog: V, back: boolean, arg: Term, name: string): Call => ({
  k: 'call',
  prog,
  back,
  arg,
  id: fresh(),
  pod: fresh(),
  name,
})

export function ofV(v: V): Term {
  return v.tag === 'leaf' ? leaf() : v.tag === 'stem' ? stem(ofV(v.c)) : fork(ofV(v.l), ofV(v.r))
}
/** The value a term stands for (finished steps are see-through), or null while anything is pending. */
export function toV(t: Term): V | null {
  switch (t.k) {
    case 'leaf':
      return VL()
    case 'stem': {
      const c = toV(t.c)
      return c && VS(c)
    }
    case 'fork': {
      const l = toV(t.l)
      const r = l && toV(t.r)
      return l && r && VF(l, r)
    }
    case 'done':
      return toV(t.body)
    default:
      return null
  }
}
export const children = (t: Term): Term[] =>
  t.k === 'stem'
    ? [t.c]
    : t.k === 'fork'
      ? [t.l, t.r]
      : t.k === 'call'
        ? [t.arg]
        : t.k === 'let'
          ? t.binds.map((b) => b.t)
          : t.k === 'done'
            ? [t.body]
            : []
export const isValue = (t: Term): boolean =>
  t.k === 'call' || t.k === 'let' ? false : children(t).every(isValue)
/** Look through finished steps to the shape underneath. */
export const bare = (t: Term): Term => (t.k === 'done' ? bare(t.body) : t)

const runDir = (prog: V, back: boolean, x: V): V | null => {
  try {
    return run(back ? VF(VL(), prog) : prog, x)
  } catch {
    return null
  }
}

// ---- one step, forward and back -------------------------------------------------------------------

class Unbound extends Error {}

interface Ctx {
  me: V
  meName: string
  names?: string[]
}

/** Which program a call pattern runs: the clause list itself, a program held in a variable, or a literal. */
function resolve(f: V, env: Env, c: Ctx): { prog: V; name: string } {
  if (f.tag === 'leaf') return { prog: c.me, name: c.meName }
  if (f.tag === 'fork' && f.l.tag === 'leaf') {
    const b = env.get(key(f.r))
    const v = b && toV(b.t)
    if (!v) throw new Unbound()
    return { prog: v, name: progName(v) ?? nameFor(f.r, c.names) }
  }
  return { prog: f, name: progName(f) ?? 'f' }
}

/** Match p against t. A call pattern f(q) is matched by running f backward on what is there: with
 * holes, that run is left as a pending call (paired with q) instead of being done now. */
function match(p: V, t: Term, env: Env, c: Ctx, holes: { p: V; t: Term }[] | null): boolean {
  const s = patShape(p)
  if (s.k === 'var') {
    const k = key(s.n)
    const had = env.get(k)
    if (had) {
      const a = toV(had.t)
      const b = toV(t)
      return !!a && !!b && eq(a, b)
    }
    env.set(k, { n: s.n, t })
    return true
  }
  if (s.k === 'call') {
    const r = resolve(s.f, env, c)
    if (holes) {
      holes.push({ p: s.p, t: mkCall(r.prog, !s.back, t, s.back ? r.name : r.name + '†') })
      return true
    }
    const v = toV(t)
    const z = v && runDir(r.prog, !s.back, v)
    return !!z && match(s.p, ofV(z), env, c, null)
  }
  const u = bare(t)
  if (s.k === 'leaf') return u.k === 'leaf'
  if (s.k === 'stem') return u.k === 'stem' && match(s.p, u.c, env, c, holes)
  if (u.k !== 'fork') return false
  const saved = new Map(env)
  const mark = holes?.length ?? 0
  try {
    return match(s.a, u.l, env, c, holes) && match(s.b, u.r, env, c, holes)
  } catch (e) {
    if (!(e instanceof Unbound)) throw e
    // a call on the left runs a program bound on the right: match the right first
    env.clear()
    for (const [k, v] of saved) env.set(k, v)
    if (holes) holes.length = mark
    return match(s.b, u.r, env, c, holes) && match(s.a, u.l, env, c, holes)
  }
}

/** A value an enclosing call has taken in: the finished steps inside it are now part of that
 * call's own record (undoing the call brings them back), so they are no longer drawn. */
function settled(t: Term): Term {
  if (t.k === 'done') return settled(t.body)
  if (t.k === 'stem') {
    const c = settled(t.c)
    return c === t.c ? t : { ...t, c }
  }
  if (t.k === 'fork') {
    const l = settled(t.l)
    const r = settled(t.r)
    return l === t.l && r === t.r ? t : { ...t, l, r }
  }
  return t
}

function build(p: V, env: Env, used: Set<string>, c: Ctx): Term {
  const s = patShape(p)
  switch (s.k) {
    case 'leaf':
      return leaf()
    case 'stem':
      return stem(build(s.p, env, used, c))
    case 'fork':
      return fork(build(s.a, env, used, c), build(s.b, env, used, c))
    case 'var': {
      const k = key(s.n)
      const b = env.get(k)!
      if (used.has(k)) return ofV(toV(b.t)!)
      used.add(k)
      return settled(b.t)
    }
    case 'call': {
      const arg = build(s.p, env, used, c)
      const r = resolve(s.f, env, c)
      return mkCall(r.prog, s.back, arg, s.back ? r.name + '†' : r.name)
    }
  }
}

/** Match the side, then build the other: now if nothing is left to wait for, else a match node. */
function settle(binds: { p: V; t: Term }[], env: Env, out: V, c: Ctx, rule: string): Term {
  const holes: { p: V; t: Term }[] = []
  for (const b of binds) if (!match(b.p, b.t, env, c, holes)) throw new Error('a match the run said would fit did not')
  if (holes.length) return { k: 'let', me: c.me, meName: c.meName, names: c.names, binds: holes, env, out, rule, id: fresh() }
  return build(out, env, new Set(), c)
}

/** What one step of a ready call or match gives. */
export function stepOf(n: Call | Let): { body: Term; rule: string } | null {
  if (n.k === 'let') return { body: settle(n.binds, new Map(n.env), n.out, n, n.rule), rule: n.rule }
  const x = toV(n.arg)
  if (!x || !runDir(n.prog, n.back, x)) return null
  const fwd = !n.back
  const s = progShape(n.prog)
  switch (s.k) {
    case 'id':
      return { body: n.arg, rule: '△' }
    case 'pair': {
      if (fwd) return { body: fork(ofV(s.a), n.arg), rule: 'a' }
      const u = bare(n.arg)
      return u.k === 'fork' ? { body: u.r, rule: 'a' } : null
    }
    case 'dagger':
      return { body: mkCall(s.b, fwd, n.arg, n.name.endsWith('†') ? n.name.slice(0, -1) : n.name + '†'), rule: '†' }
    case 'compose': {
      const [first, second] = fwd ? [s.a, s.b] : [s.b, s.a]
      const inner = mkCall(first, n.back, n.arg, progName(first) ?? 'a')
      return { body: mkCall(second, n.back, inner, progName(second) ?? 'b'), rule: '∘' }
    }
    case 'clause': {
      let cur = n.prog
      for (let i = 0; ; i++) {
        const sh = progShape(cur)
        if (sh.k !== 'clause') {
          if (cur.tag === 'leaf') return { body: n.arg, rule: '△' }
          return { body: mkCall(cur, n.back, n.arg, n.name), rule: '…' }
        }
        const c: Ctx = { me: n.prog, meName: n.name.replace(/†$/, ''), names: clauseNames(sh.l, sh.r) }
        const [inP, outP] = fwd ? [sh.l, sh.r] : [sh.r, sh.l]
        let fits = false
        try {
          fits = match(inP, n.arg, new Map(), c, null)
        } catch (e) {
          if (!(e instanceof Unbound)) throw e
        }
        // the run has already said this call does not get stuck, so the first clause that fits is the one
        if (fits) return { body: settle([{ p: inP, t: n.arg }], new Map(), outP, c, String(i + 1)), rule: String(i + 1) }
        cur = sh.rest
      }
    }
  }
}

export function fire(n: Call | Let, t: number): Done | null {
  const s = stepOf(n)
  return s && { k: 'done', body: s.body, was: n, rule: s.rule, t, id: n.id }
}

/** Put back what the step replaced. For a call, running it backward on what it gave must give back
 * what it took, or the undo is refused. */
export function unfire(d: Done): Call | Let | null {
  const w = d.was
  if (w.k === 'call') {
    const y = toV(d.body)
    const x = toV(w.arg)
    if (y && x) {
      const back = runDir(w.prog, !w.back, y)
      if (!back || !eq(back, x)) return null
    }
  }
  return w
}

// ---- the frontier: what can move now ---------------------------------------------------------------

export function find(t: Term, id: number): Term | null {
  if (t.id === id) return t
  for (const c of children(t)) {
    const f = find(c, id)
    if (f) return f
  }
  return null
}

const readyCache = new WeakMap<Call, boolean>()
/** A call can fire once its argument is a value and the run says it will not get stuck; a match once
 * every call it waits on is a value. */
export function isReady(n: Call | Let): boolean {
  if (n.k === 'let') return n.binds.every((b) => isValue(b.t))
  let r = readyCache.get(n)
  if (r === undefined) {
    const x = toV(n.arg)
    r = !!x && !!runDir(n.prog, n.back, x)
    readyCache.set(n, r)
  }
  return r
}
export const isStuck = (n: Call): boolean => isValue(n.arg) && !isReady(n)

/** Every call or match in t that can take its step now. */
export function readyIn(t: Term, out: (Call | Let)[] = []): (Call | Let)[] {
  if ((t.k === 'call' || t.k === 'let') && isReady(t)) {
    out.push(t)
    return out
  }
  for (const c of children(t)) readyIn(c, out)
  return out
}

/** Every finished step in scope that can be undone now: nothing inside it, and nothing holding it,
 * happened later. */
export function undoableIn(root: Term, scope: Term = root): Done[] {
  const out: Done[] = []
  const inScope = new Set<number>()
  const mark = (t: Term) => {
    inScope.add(t.id)
    children(t).forEach(mark)
  }
  mark(scope)
  const latest = new Map<number, number>()
  const walk = (t: Term): number => {
    let m = -1
    for (const c of children(t)) m = Math.max(m, walk(c))
    if (t.k === 'done') m = Math.max(m, t.t)
    latest.set(t.id, m)
    return m
  }
  walk(root)
  const visit = (t: Term, above: number) => {
    if (t.k === 'done') {
      if (inScope.has(t.id) && t.t > above && t.t > (latest.get(t.body.id) ?? -1)) out.push(t)
      above = Math.max(above, t.t)
    }
    for (const c of children(t)) visit(c, above)
  }
  visit(root, -1)
  return out
}

function replace(t: Term, with_: Map<number, Term>): Term {
  const r = with_.get(t.id)
  if (r) return r
  switch (t.k) {
    case 'stem': {
      const c = replace(t.c, with_)
      return c === t.c ? t : { ...t, c }
    }
    case 'fork': {
      const l = replace(t.l, with_)
      const rr = replace(t.r, with_)
      return l === t.l && rr === t.r ? t : { ...t, l, r: rr }
    }
    case 'call': {
      const arg = replace(t.arg, with_)
      return arg === t.arg ? t : { ...t, arg }
    }
    case 'let': {
      const ts = t.binds.map((b) => replace(b.t, with_))
      return ts.every((x, i) => x === t.binds[i].t) ? t : { ...t, binds: t.binds.map((b, i) => ({ p: b.p, t: ts[i] })) }
    }
    case 'done': {
      const body = replace(t.body, with_)
      return body === t.body ? t : { ...t, body }
    }
    default:
      return t
  }
}

export interface Machine {
  root: Term
  /** Each step's firings share a time; undo goes latest first. */
  clock: number
  fired: number
}

export const start = (prog: V, x: V): Machine => ({
  root: mkCall(prog, false, ofV(x), progName(prog) ?? 'f'),
  clock: 0,
  fired: 0,
})

/** Take these steps at once. */
export function fireAll(m: Machine, ns: (Call | Let)[]): Machine {
  const t = m.clock + 1
  const map = new Map<number, Term>()
  for (const n of ns) {
    const d = fire(n, t)
    if (d) map.set(n.id, d)
  }
  if (!map.size) return m
  return { root: replace(m.root, map), clock: t, fired: m.fired + map.size }
}
/** Undo these finished steps at once. */
export function undoAll(m: Machine, ds: Done[]): Machine {
  const map = new Map<number, Term>()
  for (const d of ds) {
    const w = unfire(d)
    if (w) map.set(d.id, w)
  }
  if (!map.size) return m
  return { root: replace(m.root, map), clock: m.clock, fired: m.fired - map.size }
}

/** One parallel step forward or back, inside the subtree at id (the whole tree if null). */
export function stepIn(m: Machine, id: number | null, dir: 'fwd' | 'bwd'): Machine {
  const scope = id === null ? m.root : find(m.root, id)
  if (!scope) return m
  return dir === 'fwd' ? fireAll(m, readyIn(scope)) : undoAll(m, undoableIn(m.root, scope))
}

/** Run to the end with everything ready stepping at once: how many steps fired, in how many rounds. */
export function measure(prog: V, x: V, limit = 200_000): { value: V | null; fired: number; rounds: number } {
  let m = start(prog, x)
  let rounds = 0
  for (;;) {
    const r = readyIn(m.root)
    if (!r.length || m.fired > limit) break
    m = fireAll(m, r)
    rounds++
  }
  return { value: toV(m.root), fired: m.fired, rounds }
}

const ruleCache = new WeakMap<Call | Let, string>()
/** The mark a ready call or match wears: the clause that will fire (1, 2, …), △ when it passes x
 * through, † a dagger, ∘ a composition, a a held value, ⇄ a match. */
export function ruleOf(n: Call | Let): string {
  let r = ruleCache.get(n)
  if (r === undefined) {
    r = n.k === 'let' ? '⇄' : isReady(n) ? (stepOf(n)?.rule ?? '?') : ''
    ruleCache.set(n, r)
  }
  return r
}
