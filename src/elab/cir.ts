// CIR + bracket abstraction: the Expr-independent core that turns a lambda
// IR into a tree. Bracket abstraction defines which tree a binder becomes —
// i.e. it is part of definitional equality — so changes here change every
// compiled program (the deleted lib/elab/bracket.disp was the in-language
// spec; recover via git when re-validating).

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

export function containsFree(e: Cir, name: string): boolean {
  switch (e.tag) {
    case "lit": case "S": case "K": case "I": return false
    case "var": return e.name === name
    case "app": return containsFree(e.f, name) || containsFree(e.x, name)
    case "lam": return e.x === name ? false : containsFree(e.body, name)
  }
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
function abstractName(name: string, body: Cir): Cir {
  if (!containsFree(body, name)) return cap(K, body)
  switch (body.tag) {
    case "var": return I_CIR
    case "app": {
      // η-optimization: [x](f x) where x ∉ f → f
      if (body.x.tag === "var" && body.x.name === name && !containsFree(body.f, name))
        return body.f

      const af = abstractName(name, body.f)
      const ax = abstractName(name, body.x)

      // S (K p) I → p  (η-reduction)
      if (af.tag === "app" && af.f.tag === "K" && ax.tag === "I") return af.x

      // S (K p) (K q) → K (p q)  (K-composition: compile-time eval)
      if (af.tag === "app" && af.f.tag === "K" && ax.tag === "app" && ax.f.tag === "K")
        return cap(K, cap(af.x, ax.x))

      return cap(cap(S, af), ax)
    }
    case "lam": return abstractName(name, abstractName(body.x, body.body))
    case "lit": case "S": case "K": case "I":
      throw new Error("abstract: unreachable (containsFree returned false)")
  }
}

export function eliminateLams(e: Cir): Cir {
  switch (e.tag) {
    case "lit": case "var": case "S": case "K": case "I": return e
    case "app": return cap(eliminateLams(e.f), eliminateLams(e.x))
    case "lam": return abstractName(e.x, eliminateLams(e.body))
  }
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
