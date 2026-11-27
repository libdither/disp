// CIR + bracket abstraction: the Expr-independent core that turns a lambda
// IR into a tree. Bracket abstraction defines which tree a binder becomes —
// i.e. it is part of definitional equality — so changes here change every
// compiled program (the deleted lib/elab/bracket.disp was the in-language
// spec; recover via git when re-validating). A chain of COLLECT_MIN or more
// binders compiles as an argument collector instead (see `collect`).

import type { Tree } from "../eval/eager.js"
import { elab, B } from "./state.js"

// CIR: intermediate representation with explicit S/K/I sentinels.
export type Cir =
  | { tag: "lit"; t: Tree }
  | { tag: "var"; name: string }
  | { tag: "app"; f: Cir; x: Cir }
  | { tag: "lam"; x: string; body: Cir }
  | { tag: "S" } | { tag: "K" } | { tag: "I" }

export const S: Cir = { tag: "S" }
export const K: Cir = { tag: "K" }
export const I_CIR: Cir = { tag: "I" }
export const cap = (f: Cir, x: Cir): Cir => ({ tag: "app", f, x })

const NO_FREE: ReadonlySet<string> = new Set()
/// Free variables of `e`, memoized per node: abstraction asks at every node, once per binder.
const freeMemo = new WeakMap<Cir, ReadonlySet<string>>()
function freeVars(e: Cir): ReadonlySet<string> {
  switch (e.tag) {
    case "lit": case "S": case "K": case "I": return NO_FREE
    case "var": return new Set([e.name])
  }
  let r = freeMemo.get(e)
  if (r) return r
  if (e.tag === "app") {
    const a = freeVars(e.f), b = freeVars(e.x)
    r = a.size === 0 ? b : b.size === 0 ? a : new Set([...a, ...b])
  } else {
    const inner = freeVars(e.body)
    r = inner.has(e.x) ? new Set([...inner].filter(v => v !== e.x)) : inner
  }
  freeMemo.set(e, r)
  return r
}

export function containsFree(e: Cir, name: string): boolean {
  return freeVars(e).has(name)
}

// Collect free variable names in a Cir term (in stable insertion order),
// respecting lambda-bound shadowing. Names already resolved to closed `lit`
// nodes by exprToCir don't appear (only unresolved `var` nodes are free).
// Used by the `match` desugarer to lift each arm into a closed function
// whose parameter list captures exactly the free vars across both arms.
export function collectFreeVars(e: Cir, bound: Set<string>, out: string[], seen: Set<string>): void {
  switch (e.tag) {
    case "lit": case "S": case "K": case "I": return
    case "var":
      if (!bound.has(e.name) && !seen.has(e.name)) { out.push(e.name); seen.add(e.name) }
      return
    case "app":
      collectFreeVars(e.f, bound, out, seen)
      collectFreeVars(e.x, bound, out, seen)
      return
    case "lam": {
      const inner = new Set(bound); inner.add(e.x)
      collectFreeVars(e.body, inner, out, seen)
      return
    }
  }
}

// [name]body — bracket abstraction of one binder, with the rewrites that keep
// the emitted tree small. Every rewrite is part of definitional equality
// (deterministic: same body always yields the same tree):
//   x ∉ body               → K body
//   [x]x                   → I
//   [x](f x) with x ∉ f    → f          (η, before recursing into the app)
//   S (K p) I              → p          (η, after recursing)
//   S (K p) (K q)          → K (p q)    (K-composition — evaluates at compile time)
// The lam case is nested abstraction, innermost binder first.
export type Abstract = (name: string, body: Cir) => Cir

/// `rec` is the recursive call, so a viewer can watch the recursion; the rewrites stay here.
export function abstractName(name: string, body: Cir, rec: Abstract = abstractName): Cir {
  if (!containsFree(body, name)) return cap(K, body)
  switch (body.tag) {
    case "var": return I_CIR
    case "app": {
      // η-optimization: [x](f x) where x ∉ f → f
      if (body.x.tag === "var" && body.x.name === name && !containsFree(body.f, name))
        return body.f

      const af = rec(name, body.f)
      const ax = rec(name, body.x)

      // S (K p) I → p  (η-reduction)
      if (af.tag === "app" && af.f.tag === "K" && ax.tag === "I") return af.x

      // S (K p) (K q) → K (p q)  (K-composition: compile-time eval)
      if (af.tag === "app" && af.f.tag === "K" && ax.tag === "app" && ax.f.tag === "K")
        return cap(K, cap(af.x, ax.x))

      return cap(cap(S, af), ax)
    }
    case "lam": return rec(name, rec(body.x, body.body))
    case "lit": case "S": case "K": case "I":
      throw new Error("abstract: unreachable (containsFree returned false)")
  }
}

// A chain of at least this many binders compiles as an argument collector (`collect`).
export const COLLECT_MIN = 7

export function eliminateLams(e: Cir): Cir {
  switch (e.tag) {
    case "lit": case "var": case "S": case "K": case "I": return e
    case "app": return cap(eliminateLams(e.f), eliminateLams(e.x))
    case "lam": {
      const xs: string[] = []
      let b: Cir = e
      while (b.tag === "lam") { xs.push(b.x); b = b.body }
      const body = eliminateLams(b)
      return xs.length >= COLLECT_MIN ? collect(xs, body) : curry(xs, body)
    }
  }
}

const curry = (xs: string[], body: Cir): Cir => xs.reduceRight<Cir>((acc, x) => abstractName(x, acc), body)
const lit = (t: Tree): Cir => ({ tag: "lit", t })

// fst/snd by triage: △ (△ L L) b (△ s t) → b s t, with b = K (fst) or K I (snd).
// snd must not be the 2-step `△(△(△ L))`: that is an S-rule `K t (s t)` whose `s t` only
// the evaluators' K-discard skips; a strict in-language interpreter (the archived live
// kernel's walker) evaluates `s t`, i.e. applies one env slot to the next.
// Trees are session handles, so the projections are built once per session.
const projBySession = new WeakMap<object, { fst: Tree; snd: Tree }>()
function projections(): { fst: Tree; snd: Tree } {
  let p = projBySession.get(elab.cs)
  if (!p) {
    const cs = elab.cs, tri = cs.fork(cs.leaf(), cs.leaf())
    p = { fst: cs.fork(tri, cs.stem(cs.leaf())), snd: cs.fork(tri, cs.fork(cs.leaf(), cirToTree(I_CIR))) }
    projBySession.set(cs, p)
  }
  return p
}

function substVars(e: Cir, m: Map<string, Cir>): Cir {
  switch (e.tag) {
    case "var": return m.get(e.name) ?? e
    case "app": return cap(substVars(e.f, m), substVars(e.x, m))
    case "lam": throw new Error("substVars: lambda left after elimination")
    default: return e
  }
}

let envCounter = 0

/// `{x1 … xn} => body` as an argument collector. Curried, every partial application
/// rebuilds the rest of the function with that argument substituted; here the curried
/// wrapper only packs the used arguments into a balanced tuple, and the body is
/// abstracted over that one tuple, reading each variable back by fst/snd.
function collect(xs0: string[], body0: Cir): Cir {
  const xs = [...xs0]
  let body = body0
  while (xs.length > 0) {
    const x = xs[xs.length - 1]
    if (body.tag === "app" && body.x.tag === "var" && body.x.name === x && !containsFree(body.f, x)) { body = body.f; xs.pop() }
    else break
  }
  // last binder of each name wins (shadowing); keep only used ones
  const used: string[] = []
  for (let i = xs.length - 1; i >= 0; i--)
    if (!used.includes(xs[i]) && containsFree(body, xs[i]) && xs.lastIndexOf(xs[i]) === i) used.unshift(xs[i])
  if (used.length < 2) return curry(xs, body)
  const { fst, snd } = projections()
  const env = `__env${envCounter++}`
  const read = new Map<string, Cir>()
  const pack = (lo: number, hi: number, at: (c: Cir) => Cir): Cir => {
    if (hi - lo === 1) { read.set(used[lo], at({ tag: "var", name: env })); return { tag: "var", name: used[lo] } }
    const mid = (lo + hi) >> 1
    const l = pack(lo, mid, c => cap(lit(fst), at(c)))
    const r = pack(mid, hi, c => cap(lit(snd), at(c)))
    return cap(cap(lit(elab.cs.leaf()), l), r)
  }
  const tuple = pack(0, used.length, c => c)
  return curry(xs, cap(abstractName(env, substVars(body, read)), tuple))
}

export function cirToTree(e: Cir): Tree {
  switch (e.tag) {
    case "lit": return e.t
    case "var": throw new Error(`cirToTree: unresolved free variable ${e.name}`)
    case "I":   return elab.cs.fork(elab.cs.fork(elab.cs.leaf(), elab.cs.leaf()), elab.cs.leaf())
    case "K":   return elab.cs.stem(elab.cs.leaf())
    case "S":   return elab.cs.fork(elab.cs.stem(elab.cs.fork(elab.cs.leaf(), elab.cs.leaf())), elab.cs.leaf())
    case "lam": throw new Error(`cirToTree: unexpected lambda for ${e.x}`)
    case "app": {
      if (e.f.tag === "app" && e.f.f.tag === "S") return elab.cs.fork(elab.cs.stem(cirToTree(e.f.x)), cirToTree(e.x))
      // Full K application: K(x)(y) → x (drop second arg)
      if (e.f.tag === "app" && e.f.f.tag === "K") return cirToTree(e.f.x)
      if (e.f.tag === "K") return elab.cs.fork(elab.cs.leaf(), cirToTree(e.x))
      if (e.f.tag === "I") return cirToTree(e.x)
      // Partial S application: S(x) → stem(stem(x)) so that S(x)(y) = fork(stem(x), y)
      if (e.f.tag === "S") return elab.cs.stem(elab.cs.stem(cirToTree(e.x)))
      return elab.cs.apply(cirToTree(e.f), cirToTree(e.x), B())
    }
  }
}
