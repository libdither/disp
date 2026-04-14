// Surface elaborator: typed surface syntax → tagged-form tree.
//
// Mirrors the encoding in examples/predicates.disp:
//   tagged forms (V, H, App, Lam, Pi) with explicit binders, type
//   annotations, and bind-trees. The output is a tagged tree the disp
//   programs can pred_of-check and erase to runtime.
//
// Per philosophy rule 2: this is the host-side mirror; tree-program port
// is planned for a later phase.
//
// Algorithm: each annotated lambda \(x : T). body introduces a fresh
// `mkH T marker` token for x in the body's environment. After elaborating
// the body, computeBind walks the body looking for that token and produces
// the bind-tree marking its positions; then replaceMarker swaps the token
// for V. Same for Pi codoms.

import { Tree, LEAF, stem, fork, isLeaf, isStem, isFork } from "./tree.js"

// ===== Tagged-form encoding (mirrors examples/predicates.disp) =====

const TAG_ROOT = stem(stem(stem(stem(stem(stem(stem(LEAF)))))))
const tagged = (k: Tree, p: Tree) => fork(fork(TAG_ROOT, k), p)

const KV = LEAF
const KH = stem(LEAF)
const KA = stem(stem(LEAF))
const KL = stem(stem(stem(LEAF)))
const KP = stem(stem(stem(stem(LEAF))))

export const V = tagged(KV, LEAF)
export const mkH = (ty: Tree, lvl: Tree): Tree => tagged(KH, fork(ty, lvl))
export const mkApp = (f: Tree, x: Tree): Tree => tagged(KA, fork(f, x))
export const mkLam = (A: Tree, B: Tree, body: Tree): Tree => tagged(KL, fork(fork(A, B), body))
export const mkPi = (A: Tree, B: Tree, cod: Tree): Tree => tagged(KP, fork(fork(A, B), cod))

export const BE = LEAF
export const BN = stem(LEAF)
const KBA = stem(stem(LEAF))
const KBL = stem(stem(stem(LEAF)))
const KBP = stem(stem(stem(stem(LEAF))))
const mkBApp = (l: Tree, r: Tree): Tree => fork(KBA, fork(l, r))
const mkBLam = (bA: Tree, bBody: Tree): Tree => fork(KBL, fork(bA, bBody))
const mkBPi  = (bA: Tree, bCod: Tree): Tree => fork(KBP, fork(bA, bCod))

// Kind dispatch (structural; mirrors is_kind in disp)
const isTagged = (t: Tree): boolean => isFork(t) && isFork(t.left) && t.left.left.id === TAG_ROOT.id
const kindOf = (t: Tree): Tree | null => isTagged(t) ? t.left.right : null
const isAppT = (t: Tree) => kindOf(t)?.id === KA.id
const isLamT = (t: Tree) => kindOf(t)?.id === KL.id
const isPiT  = (t: Tree) => kindOf(t)?.id === KP.id

// Accessors (assume tagged form)
const payload = (t: Tree): Tree => (t as { right: Tree }).right
const appF_ = (t: Tree) => (payload(t) as { left: Tree }).left
const appX_ = (t: Tree) => (payload(t) as { right: Tree }).right
const lamA_ = (t: Tree) => ((payload(t) as { left: Tree }).left as { left: Tree }).left
const lamB_ = (t: Tree) => ((payload(t) as { left: Tree }).left as { right: Tree }).right
const lamBody_ = (t: Tree) => (payload(t) as { right: Tree }).right
const piA_ = (t: Tree) => ((payload(t) as { left: Tree }).left as { left: Tree }).left
const piB_ = (t: Tree) => ((payload(t) as { left: Tree }).left as { right: Tree }).right
const piCodom_ = (t: Tree) => (payload(t) as { right: Tree }).right

// ===== Surface IR =====

export type Surface =
  | { tag: "leaf" }
  | { tag: "var"; name: string }
  | { tag: "app"; f: Surface; x: Surface }
  | { tag: "alam"; x: string; type: Surface; body: Surface }
  | { tag: "pi"; x: string; dom: Surface; cod: Surface }
  | { tag: "arrow"; dom: Surface; cod: Surface }
  | { tag: "raw"; tree: Tree }

// ===== Fresh-marker source =====
// Hand-constructed and elaborator-minted markers default to leaf-rooted
// stem chains. Descent markers (in pred_of) use fork-rooted shapes —
// disjoint namespace. Counter persists across compilation to keep markers
// pairwise distinct within a session.

let markerCounter = 0
export function freshMarker(): Tree {
  markerCounter++
  let t = LEAF
  for (let i = 0; i < markerCounter; i++) t = stem(t)
  return t
}
export function resetMarkerCounter(): void { markerCounter = 0 }

// ===== Environment =====
//
// Maps variable names to the tree to substitute when that name is
// referenced. For lambda-bound vars, that tree is the fresh H-token;
// for globals, it's the global's value. The elaborator doesn't track
// types separately — type-checking is downstream via pred_of.

export type Env = Map<string, Tree>

// ===== compute_bind: where does `token` appear in `term`? =====

function computeBind(term: Tree, token: Tree): Tree {
  if (term.id === token.id) return BE
  if (isAppT(term)) {
    const bf = computeBind(appF_(term), token)
    const bx = computeBind(appX_(term), token)
    if (bf.id === BN.id && bx.id === BN.id) return BN
    return mkBApp(bf, bx)
  }
  if (isLamT(term)) {
    const bA = computeBind(lamA_(term), token)
    const bBody = computeBind(lamBody_(term), token)
    if (bA.id === BN.id && bBody.id === BN.id) return BN
    return mkBLam(bA, bBody)
  }
  if (isPiT(term)) {
    const bA = computeBind(piA_(term), token)
    const bCod = computeBind(piCodom_(term), token)
    if (bA.id === BN.id && bCod.id === BN.id) return BN
    return mkBPi(bA, bCod)
  }
  return BN
}

// Replace every occurrence of `token` in `term` with `replacement`.
// Walks tagged structure; non-tagged subtrees pass through.
function replaceMarker(term: Tree, token: Tree, replacement: Tree): Tree {
  if (term.id === token.id) return replacement
  if (isAppT(term)) return mkApp(replaceMarker(appF_(term), token, replacement), replaceMarker(appX_(term), token, replacement))
  if (isLamT(term)) return mkLam(replaceMarker(lamA_(term), token, replacement), lamB_(term), replaceMarker(lamBody_(term), token, replacement))
  if (isPiT(term)) return mkPi(replaceMarker(piA_(term), token, replacement), piB_(term), replaceMarker(piCodom_(term), token, replacement))
  return term
}

// ===== elab: surface → tagged tree =====
//
// The elaborator only produces the term. Type-checking is downstream via
// pred_of in disp tests. For annotated lambdas, the body's bind-tree is
// computed automatically from where the introduced H-marker appears.

export function elab(s: Surface, env: Env): Tree {
  switch (s.tag) {
    case "leaf": return LEAF
    case "var": {
      const tree = env.get(s.name)
      if (!tree) throw new Error(`elab: unbound var '${s.name}'`)
      return tree
    }
    case "app": return mkApp(elab(s.f, env), elab(s.x, env))
    case "alam": {
      const T = elab(s.type, env)
      const token = mkH(T, freshMarker())
      const newEnv = new Map(env)
      newEnv.set(s.x, token)
      const body = elab(s.body, newEnv)
      const bBody = computeBind(body, token)
      const bodyV = replaceMarker(body, token, V)
      return mkLam(T, bBody, bodyV)
    }
    case "pi": {
      const dom = elab(s.dom, env)
      const token = mkH(dom, freshMarker())
      const newEnv = new Map(env)
      newEnv.set(s.x, token)
      const cod = elab(s.cod, newEnv)
      const bCod = computeBind(cod, token)
      const codV = replaceMarker(cod, token, V)
      return mkPi(dom, bCod, codV)
    }
    case "arrow": {
      const dom = elab(s.dom, env)
      const cod = elab(s.cod, env)
      return mkPi(dom, BN, cod)
    }
    case "raw": return s.tree
  }
}
