// Calculus of Constructions encoded directly in tree calculus.
//
// Terms are encoded as trees:
//   Type         = LEAF
//   Var(marker)  = stem(marker)
//   App(M, N)    = fork(LEAF, fork(M, N))
//   Lam(A, body) = fork(stem(A), body)        -- body is bracket-abstracted
//   Pi(A, body)  = fork(fork(A, LEAF), body)   -- body is bracket-abstracted
//
// Each value is "wrapped" as a Church pair carrying (data, type):
//   WRAP(data, type) = S(S I (K data))(K type)
//   unwrapData(w)    = apply(w, K)
//   unwrapType(w)    = apply(w, K*)

import { type Tree, LEAF, stem, fork, treeEqual, apply, I, treeApply } from "./tree.js"
import { type Expr, eTree, eFvar, eApp, bracketAbstract, collapse } from "./compile.js"
import { type SExpr, type SDecl, stype, parseLine } from "./parse.js"

// ============================================================
// Phase 1: Encoding Primitives
// ============================================================

// --- Encoding constructors ---

export function encType(): Tree { return LEAF }

export function encVar(marker: Tree): Tree { return stem(marker) }

export function encApp(m: Tree, n: Tree): Tree { return fork(LEAF, fork(m, n)) }

export function encLam(domain: Tree, body: Tree): Tree { return fork(stem(domain), body) }

export function encPi(domain: Tree, body: Tree): Tree { return fork(fork(domain, LEAF), body) }

// --- Tag recognition ---

export type TermTag = "type" | "var" | "app" | "lam" | "pi"

export function termTag(t: Tree): TermTag | null {
  if (t.tag === "leaf") return "type"
  if (t.tag === "stem") return "var"
  if (t.tag === "fork") {
    const left = t.left
    if (left.tag === "leaf") return "app"
    if (left.tag === "stem") return "lam"
    if (left.tag === "fork") return "pi"
  }
  return null
}

// --- Destructuring ---

export function unVar(t: Tree): Tree | null {
  if (t.tag === "stem") return t.child
  return null
}

export function unApp(t: Tree): { func: Tree, arg: Tree } | null {
  if (t.tag === "fork" && t.left.tag === "leaf" && t.right.tag === "fork") {
    return { func: t.right.left, arg: t.right.right }
  }
  return null
}

export function unLam(t: Tree): { domain: Tree, body: Tree } | null {
  if (t.tag === "fork" && t.left.tag === "stem") {
    return { domain: t.left.child, body: t.right }
  }
  return null
}

export function unPi(t: Tree): { domain: Tree, body: Tree } | null {
  if (t.tag === "fork" && t.left.tag === "fork" && t.left.right.tag === "leaf") {
    return { domain: t.left.left, body: t.right }
  }
  return null
}

// --- WRAP (Church pair via bracket abstraction) ---

// K selector: stem(LEAF). apply(wrap, K) → data
export const K_SEL: Tree = stem(LEAF)

// K* selector: fork(LEAF, I). apply(wrap, K*) → type
export const K_STAR_SEL: Tree = fork(LEAF, I)

export function wrap(data: Tree, type: Tree): Tree {
  const expr = eApp(eApp(eFvar("f"), eTree(data)), eTree(type))
  return collapse(bracketAbstract("f", expr))
}

export function unwrapData(wrapped: Tree): Tree {
  return apply(wrapped, K_SEL)
}

export function unwrapType(wrapped: Tree): Tree {
  return apply(wrapped, K_STAR_SEL)
}

// --- Fresh markers ---

let markerCounter = 0

export function freshMarker(): Tree {
  let t: Tree = LEAF
  for (let i = 0; i < markerCounter; i++) {
    t = stem(t)
  }
  markerCounter++
  return fork(LEAF, t)
}

export function resetMarkerCounter(): void {
  markerCounter = 0
}

// ============================================================
// Phase 2: Bracket Abstraction Integration
// ============================================================

export function treeToExprReplacing(t: Tree, target: Tree, varName: string): Expr {
  if (treeEqual(t, target)) return eFvar(varName)
  if (t.tag === "leaf") return eTree(t)
  if (t.tag === "stem") {
    const child = treeToExprReplacing(t.child, target, varName)
    if (child.tag === "tree" && treeEqual(child.value, t.child)) return eTree(t)
    return eApp(eTree(LEAF), child)
  }
  const left = treeToExprReplacing(t.left, target, varName)
  const right = treeToExprReplacing(t.right, target, varName)
  if (left.tag === "tree" && treeEqual(left.value, t.left) &&
      right.tag === "tree" && treeEqual(right.value, t.right))
    return eTree(t)
  return eApp(eApp(eTree(LEAF), left), right)
}

export function abstractMarkerOut(tree: Tree, target: Tree): Tree {
  const name = "__marker__"
  const expr = treeToExprReplacing(tree, target, name)
  return collapse(bracketAbstract(name, expr))
}

// ============================================================
// Phase 3: WHNF, Convertibility, and Core Building
// ============================================================

export class CocError extends Error {
  constructor(msg: string) { super(msg) }
}

export function whnf(t: Tree, budget = { remaining: 10000 }): Tree {
  if (budget.remaining <= 0) throw new CocError("WHNF budget exhausted")
  const app = unApp(t)
  if (!app) return t
  budget.remaining--
  const func = whnf(app.func, budget)
  const lam = unLam(func)
  if (lam) {
    const result = apply(lam.body, app.arg)
    return whnf(result, budget)
  }
  if (treeEqual(func, app.func)) return t
  return encApp(func, app.arg)
}

export function normalize(t: Tree, budget = { remaining: 100000 }): Tree {
  if (budget.remaining <= 0) throw new CocError("Normalization budget exhausted")
  budget.remaining--
  const w = whnf(t, budget)
  const tag = termTag(w)
  switch (tag) {
    case "type":
    case "var":
      return w
    case "app": {
      const a = unApp(w)!
      return encApp(normalize(a.func, budget), normalize(a.arg, budget))
    }
    case "lam": {
      const l = unLam(w)!
      const m = freshMarker()
      const bodyApplied = apply(l.body, encVar(m))
      const normBody = normalize(bodyApplied, budget)
      const normDomain = normalize(l.domain, budget)
      return encLam(normDomain, abstractMarkerOut(normBody, encVar(m)))
    }
    case "pi": {
      const p = unPi(w)!
      const m = freshMarker()
      const bodyApplied = apply(p.body, encVar(m))
      const normBody = normalize(bodyApplied, budget)
      const normDomain = normalize(p.domain, budget)
      return encPi(normDomain, abstractMarkerOut(normBody, encVar(m)))
    }
    default:
      return w
  }
}

export function convertible(a: Tree, b: Tree, budget = { remaining: 10000 }): boolean {
  if (treeEqual(a, b)) return true
  const aN = whnf(a, budget)
  const bN = whnf(b, budget)
  if (treeEqual(aN, bN)) return true
  const aTag = termTag(aN)
  const bTag = termTag(bN)
  if (aTag !== bTag) return false
  switch (aTag) {
    case "type": return true
    case "var": return treeEqual(unVar(aN)!, unVar(bN)!)
    case "app": {
      const aApp = unApp(aN)!
      const bApp = unApp(bN)!
      return convertible(aApp.func, bApp.func, budget) &&
             convertible(aApp.arg, bApp.arg, budget)
    }
    case "lam": {
      const aLam = unLam(aN)!
      const bLam = unLam(bN)!
      if (!convertible(aLam.domain, bLam.domain, budget)) return false
      return convertibleUnderBinder(aLam.body, bLam.body, budget)
    }
    case "pi": {
      const aPi = unPi(aN)!
      const bPi = unPi(bN)!
      if (!convertible(aPi.domain, bPi.domain, budget)) return false
      return convertibleUnderBinder(aPi.body, bPi.body, budget)
    }
    default: return false
  }
}

export function convertibleUnderBinder(body1: Tree, body2: Tree, budget = { remaining: 10000 }): boolean {
  const m = freshMarker()
  const neutral = encVar(m)
  const r1 = apply(body1, neutral)
  const r2 = apply(body2, neutral)
  return convertible(r1, r2, budget)
}

// ============================================================
// Phase 3 & 4: buildWrapped — the main builder
// ============================================================

export type Env = Map<string, Tree>

export function buildWrapped(sexpr: SExpr, env: Env, expectedType?: Tree, budget = { remaining: 10000 }): Tree {
  switch (sexpr.tag) {
    case "stype":
      return wrap(encType(), encType())
    case "svar": {
      const wrapped = env.get(sexpr.name)
      if (!wrapped) throw new CocError(`Unbound variable: ${sexpr.name}`)
      return wrapped
    }
    case "spi": {
      const domWrapped = buildWrapped(sexpr.domain, env, encType(), budget)
      const domData = unwrapData(domWrapped)
      const domType = unwrapType(domWrapped)
      ensureIsType(domType, `Domain of Pi type`, budget)
      const m = freshMarker()
      const neutral = encVar(m)
      const binderWrapped = wrap(neutral, domData)
      const extEnv = new Map(env)
      if (sexpr.name !== "_") extEnv.set(sexpr.name, binderWrapped)
      const codWrapped = buildWrapped(sexpr.codomain, extEnv, encType(), budget)
      const codData = unwrapData(codWrapped)
      const codType = unwrapType(codWrapped)
      ensureIsType(codType, `Codomain of Pi type`, budget)
      const abstractedCod = abstractMarkerOut(codData, neutral)
      return wrap(encPi(domData, abstractedCod), encType())
    }
    case "slam": {
      if (!expectedType) throw new CocError("Cannot infer type of lambda expression")
      if (sexpr.params.length > 1) {
        const [first, ...rest] = sexpr.params
        const innerLam: SExpr = { tag: "slam", params: rest, body: sexpr.body }
        const singleLam: SExpr = { tag: "slam", params: [first], body: innerLam }
        return buildWrapped(singleLam, env, expectedType, budget)
      }
      const param = sexpr.params[0]
      const piTree = whnf(expectedType, budget)
      const pi = unPi(piTree)
      if (!pi) throw new CocError(`Lambda needs function type, got non-Pi`)
      const domain = pi.domain
      const m = freshMarker()
      const neutral = encVar(m)
      const paramWrapped = wrap(neutral, domain)
      const expectedCod = apply(pi.body, neutral)
      const extEnv = new Map(env)
      extEnv.set(param, paramWrapped)
      const bodyWrapped = buildWrapped(sexpr.body, extEnv, expectedCod, budget)
      const bodyData = unwrapData(bodyWrapped)
      const bodyType = unwrapType(bodyWrapped)
      const abstractedBody = abstractMarkerOut(bodyData, neutral)
      const abstractedCod = abstractMarkerOut(bodyType, neutral)
      return wrap(encLam(domain, abstractedBody), encPi(domain, abstractedCod))
    }
    case "sapp": {
      const funcWrapped = buildWrapped(sexpr.func, env, undefined, budget)
      const funcData = unwrapData(funcWrapped)
      const funcType = unwrapType(funcWrapped)
      const piTree = whnf(funcType, budget)
      const pi = unPi(piTree)
      if (!pi) throw new CocError(`Expected function type in application`)
      const argWrapped = buildWrapped(sexpr.arg, env, pi.domain, budget)
      const argData = unwrapData(argWrapped)
      const argType = unwrapType(argWrapped)
      if (!convertible(argType, pi.domain, budget)) {
        throw new CocError(`Type mismatch in application: argument type doesn't match domain`)
      }
      const resultData = encApp(funcData, argData)
      const resultType = apply(pi.body, argData)
      return wrap(resultData, resultType)
    }
  }
}

function ensureIsType(typeTree: Tree, context: string, budget = { remaining: 10000 }): void {
  if (!convertible(typeTree, encType(), budget)) {
    throw new CocError(`${context} must be a type`)
  }
}

// ============================================================
// Phase 4: Declaration Pipeline
// ============================================================

export function cocCheckDecl(
  env: Env, name: string, type: SExpr | null, value: SExpr,
  isRec = false, budget = { remaining: 10000 }
): { env: Env, type: Tree } {
  if (isRec) return cocCheckRecDecl(env, name, type!, value, budget)
  if (type !== null) {
    const typeWrapped = buildWrapped(type, env, encType(), budget)
    const typeData = unwrapData(typeWrapped)
    ensureIsType(unwrapType(typeWrapped), `Type annotation for ${name}`, budget)
    const valWrapped = buildWrapped(value, env, typeData, budget)
    const valData = unwrapData(valWrapped)
    if (!convertible(unwrapType(valWrapped), typeData, budget)) {
      throw new CocError(`Type mismatch for ${name}`)
    }
    const newEnv = new Map(env)
    newEnv.set(name, wrap(valData, typeData))
    return { env: newEnv, type: typeData }
  } else {
    const valWrapped = buildWrapped(value, env, undefined, budget)
    const valData = unwrapData(valWrapped)
    const valType = unwrapType(valWrapped)
    const newEnv = new Map(env)
    newEnv.set(name, wrap(valData, valType))
    return { env: newEnv, type: valType }
  }
}

export function cocCheckRecDecl(
  env: Env, name: string, type: SExpr, value: SExpr,
  budget = { remaining: 10000 }
): { env: Env, type: Tree } {
  if (!type) throw new CocError(`Recursive definition '${name}' requires a type annotation`)
  const typeWrapped = buildWrapped(type, env, encType(), budget)
  const typeData = unwrapData(typeWrapped)
  ensureIsType(unwrapType(typeWrapped), `Type annotation for ${name}`, budget)
  const selfMarker = freshMarker()
  const selfNeutral = encVar(selfMarker)
  const preEnv = new Map(env)
  preEnv.set(name, wrap(selfNeutral, typeData))
  const bodyWrapped = buildWrapped(value, preEnv, typeData, budget)
  const bodyData = unwrapData(bodyWrapped)
  const abstractedBody = abstractMarkerOut(bodyData, selfNeutral)
  const xMarker = freshMarker()
  const xNeutral = encVar(xMarker)
  const vMarker = freshMarker()
  const vNeutral = encVar(vMarker)
  const selfApp = apply(apply(xNeutral, xNeutral), vNeutral)
  const thunk = abstractMarkerOut(selfApp, vNeutral)
  const bodyWithThunk = apply(abstractedBody, thunk)
  const omega = abstractMarkerOut(bodyWithThunk, xNeutral)
  const result = treeApply(omega, omega)
  const newEnv = new Map(env)
  newEnv.set(name, wrap(result, typeData))
  return { env: newEnv, type: typeData }
}

// ============================================================
// Phase 5: Pretty Printing
// ============================================================

let printVarCounter = 0
const PRINT_VAR_NAMES = "abcdefghijklmnopqrstuvwxyz"

function freshPrintVar(): string {
  const idx = printVarCounter++
  if (idx < 26) return PRINT_VAR_NAMES[idx]
  return `v${idx}`
}

export function printEncoded(t: Tree, nameMap?: Map<number, string>, budget = { remaining: 10000 }): string {
  const savedCounter = printVarCounter
  printVarCounter = 0
  const result = printEncodedInner(t, nameMap, budget)
  printVarCounter = savedCounter
  return result
}

function printEncodedInner(t: Tree, nameMap?: Map<number, string>, budget = { remaining: 10000 }): string {
  if (nameMap) { const name = nameMap.get(t.id); if (name) return name }
  const w = whnf(t, budget)
  if (nameMap && w.id !== t.id) { const name = nameMap.get(w.id); if (name) return name }
  const tag = termTag(w)
  switch (tag) {
    case "type": return "Type"
    case "var": {
      const marker = unVar(w)!
      if (nameMap) { const name = nameMap.get(marker.id); if (name) return name }
      return `?${marker.id}`
    }
    case "app": {
      const a = unApp(w)!
      return `${printEncodedInner(a.func, nameMap, budget)} ${printEncodedAtom(a.arg, nameMap, budget)}`
    }
    case "lam": {
      const l = unLam(w)!
      const varName = freshPrintVar()
      const m = freshMarker()
      const extMap = new Map(nameMap ?? [])
      extMap.set(m.id, varName)
      const bodyApplied = apply(l.body, encVar(m))
      return `{${varName}} -> ${printEncodedInner(bodyApplied, extMap, budget)}`
    }
    case "pi": {
      const p = unPi(w)!
      const m = freshMarker()
      const neutral = encVar(m)
      const bodyApplied = apply(p.body, neutral)
      const domStr = printEncodedInner(p.domain, nameMap, budget)
      const isDep = treeContains(bodyApplied, neutral)
      if (isDep) {
        const varName = freshPrintVar()
        const extMap = new Map(nameMap ?? [])
        extMap.set(m.id, varName)
        return `(${varName} : ${domStr}) -> ${printEncodedInner(bodyApplied, extMap, budget)}`
      }
      return `(${domStr}) -> ${printEncodedInner(bodyApplied, nameMap, budget)}`
    }
    default:
      return `<tree:${w.id}>`
  }
}

function printEncodedAtom(t: Tree, nameMap?: Map<number, string>, budget = { remaining: 10000 }): string {
  if (nameMap) { const name = nameMap.get(t.id); if (name) return name }
  const tag = termTag(t)
  if (tag === "type" || tag === "var") return printEncodedInner(t, nameMap, budget)
  return `(${printEncodedInner(t, nameMap, budget)})`
}

function treeContains(tree: Tree, target: Tree): boolean {
  if (treeEqual(tree, target)) return true
  if (tree.tag === "leaf") return false
  if (tree.tag === "stem") return treeContains(tree.child, target)
  return treeContains(tree.left, target) || treeContains(tree.right, target)
}

// ============================================================
// Tree-native core operations (actual tree constants)
// ============================================================
//
// These are TREES, not TypeScript functions. When you apply() them to
// arguments, tree calculus execution (triage) performs the computation.

// --- Expr-level helpers ---

function exprFork(a: Expr, b: Expr): Expr { return eApp(eApp(eTree(LEAF), a), b) }
function exprTriage(c: Expr, d: Expr, b: Expr): Expr { return exprFork(exprFork(c, d), b) }
function exprS(f: Expr, g: Expr): Expr { return eApp(eApp(eTree(LEAF), eApp(eTree(LEAF), f)), g) }
function exprK(c: Expr): Expr { return eApp(eTree(stem(LEAF)), c) }
function mkTriage(onLeaf: Tree, onStem: Tree, onFork: Tree): Tree { return fork(fork(onLeaf, onStem), onFork) }

function compileTree(params: string[], body: Expr): Tree {
  let e = body
  for (let i = params.length - 1; i >= 0; i--) e = bracketAbstract(params[i], e)
  return collapse(e)
}

// --- Primitive tree destructors (single triage, no recursion) ---

// FST: fork(l,r) → l. Fork handler = K.
export const FST: Tree = mkTriage(LEAF, LEAF, stem(LEAF))
// SND: fork(l,r) → r. Fork handler = K*.
export const SND: Tree = mkTriage(LEAF, LEAF, fork(LEAF, I))
// CHILD: stem(u) → u. Stem handler = I.
export const CHILD: Tree = mkTriage(LEAF, I, LEAF)

// --- Encoding constructors as tree constants ---

export const ENC_APP_T: Tree = compileTree(["m", "n"],
  eApp(eTree(stem(LEAF)), exprFork(eFvar("m"), eFvar("n"))))
export const ENC_LAM_T: Tree = compileTree(["d", "b"],
  exprFork(eApp(eTree(LEAF), eFvar("d")), eFvar("b")))
export const ENC_PI_T: Tree = compileTree(["d", "b"],
  exprFork(exprFork(eFvar("d"), eTree(LEAF)), eFvar("b")))

// --- termCase: 5-way dispatch on encoded CoC terms ---

export const TERM_CASE: Tree = (() => {
  const appCase = eApp(eApp(eFvar("onApp"), eApp(eTree(FST), eFvar("right"))),
                                              eApp(eTree(SND), eFvar("right")))
  const lamCase = bracketAbstract("__d",
    eApp(eApp(eFvar("onLam"), eFvar("__d")), eFvar("right")))
  const piCase = bracketAbstract("__dl", bracketAbstract("__",
    eApp(eApp(eFvar("onPi"), eFvar("__dl")), eFvar("right"))))
  const innerTriage = eApp(exprTriage(appCase, lamCase, piCase), eFvar("left"))
  const forkHandler = bracketAbstract("left", bracketAbstract("right", innerTriage))
  const body = eApp(exprTriage(eFvar("onType"), eFvar("onVar"), forkHandler), eFvar("term"))
  return compileTree(["onType", "onVar", "onApp", "onLam", "onPi", "term"], body)
})()

// --- Tree-native equality (recursive via omega combinator) ---

const TRUE_T: Tree = stem(LEAF)       // K: K(t)(f) = t
const FALSE_T: Tree = fork(LEAF, I)   // K*: K*(t)(f) = f

export const TREE_EQ: Tree = (() => {
  const self = (p: Expr, q: Expr) => eApp(eApp(eApp(eFvar("x"), eFvar("x")), p), q)
  const andE = (p: Expr, q: Expr) => eApp(eApp(p, q), eTree(FALSE_T))

  const onLeaf = eApp(exprTriage(eTree(TRUE_T),
    bracketAbstract("_s1", eTree(FALSE_T)),
    bracketAbstract("_f1", bracketAbstract("_f2", eTree(FALSE_T)))
  ), eFvar("b"))

  const onStem = bracketAbstract("ac", eApp(exprTriage(eTree(FALSE_T),
    bracketAbstract("bc", self(eFvar("ac"), eFvar("bc"))),
    bracketAbstract("_f1", bracketAbstract("_f2", eTree(FALSE_T)))
  ), eFvar("b")))

  const onFork = bracketAbstract("al", bracketAbstract("ar", eApp(exprTriage(eTree(FALSE_T),
    bracketAbstract("_s2", eTree(FALSE_T)),
    bracketAbstract("bl", bracketAbstract("br",
      andE(self(eFvar("al"), eFvar("bl")), self(eFvar("ar"), eFvar("br")))
    ))
  ), eFvar("b"))))

  const body = eApp(exprTriage(onLeaf, onStem, onFork), eFvar("a"))
  const omega = compileTree(["x", "a", "b"], body)
  return treeApply(omega, omega)
})()

// --- Tree-native bracket abstraction (recursive via omega) ---

export const ABSTRACT_OUT: Tree = (() => {
  const self = (p: Expr, q: Expr) => eApp(eApp(eApp(eFvar("x"), eFvar("x")), p), q)
  const K_LEAF = exprK(eTree(LEAF))

  const onStem = bracketAbstract("c", exprS(K_LEAF, self(eFvar("target"), eFvar("c"))))
  const onFork = bracketAbstract("l", bracketAbstract("r",
    exprS(exprS(K_LEAF, self(eFvar("target"), eFvar("l"))), self(eFvar("target"), eFvar("r")))))

  const triageOnTree = eApp(exprTriage(exprK(eTree(LEAF)), onStem, onFork), eFvar("tree"))
  const eqCheck = eApp(eApp(eTree(TREE_EQ), eFvar("tree")), eFvar("target"))
  const body = eApp(eApp(eqCheck, eTree(I)), triageOnTree)

  const omega = compileTree(["x", "target", "tree"], body)
  return treeApply(omega, omega)
})()

// ============================================================
// CoC Prelude: Church-encoded Tree and encoding operations
// ============================================================

export const COC_PRELUDE: string[] = [
  "let Tree : Type := (R : Type) -> R -> (R -> R) -> (R -> R -> R) -> R",
  "let leaf : Tree := {R c d b} -> c",
  "let stem : Tree -> Tree := {t R c d b} -> d (t R c d b)",
  "let fork : Tree -> Tree -> Tree := {l r R c d b} -> b (l R c d b) (r R c d b)",
  "let triage : (R : Type) -> R -> (R -> R) -> (R -> R -> R) -> Tree -> R := {R c d b t} -> t R c d b",
  "let encType : Tree := leaf",
  "let encVar : Tree -> Tree := stem",
  "let encApp : Tree -> Tree -> Tree := {m n} -> fork leaf (fork m n)",
  "let encLam : Tree -> Tree -> Tree := {domain body} -> fork (stem domain) body",
  "let encPi : Tree -> Tree -> Tree := {domain body} -> fork (fork domain leaf) body",
  "let Wrapped : Type := (R : Type) -> (Tree -> Tree -> R) -> R",
  "let wrap : Tree -> Tree -> Wrapped := {d t R sel} -> sel d t",
  "let unwrapData : Wrapped -> Tree := {w} -> w Tree ({d t} -> d)",
  "let unwrapType : Wrapped -> Tree := {w} -> w Tree ({d t} -> t)",
  "let Bool : Type := (R : Type) -> R -> R -> R",
  "let tt : Bool := {R t f} -> t",
  "let ff : Bool := {R t f} -> f",
]

interface TreeBuiltin { name: string; type: string; data: Tree }

export const TREE_NATIVE_BUILTINS: TreeBuiltin[] = [
  { name: "tfst",     type: "Tree -> Tree",   data: FST },
  { name: "tsnd",     type: "Tree -> Tree",   data: SND },
  { name: "tchild",   type: "Tree -> Tree",   data: CHILD },
  { name: "tEncApp",  type: "Tree -> Tree -> Tree", data: ENC_APP_T },
  { name: "tEncLam",  type: "Tree -> Tree -> Tree", data: ENC_LAM_T },
  { name: "tEncPi",   type: "Tree -> Tree -> Tree", data: ENC_PI_T },
  { name: "termCase", type: "Tree -> (Tree -> Tree) -> (Tree -> Tree -> Tree) -> (Tree -> Tree -> Tree) -> (Tree -> Tree -> Tree) -> Tree -> Tree",
    data: TERM_CASE },
  { name: "treeEq",   type: "Tree -> Tree -> Bool", data: TREE_EQ },
  { name: "abstractOut", type: "Tree -> Tree -> Tree", data: ABSTRACT_OUT },
]

export function loadCocPrelude(env: Env): Env {
  for (const decl of COC_PRELUDE) {
    const parsed = parseLine(decl)
    if (!("isRec" in parsed)) continue
    const sdecl = parsed as SDecl
    const result = cocCheckDecl(env, sdecl.name, sdecl.type, sdecl.value, sdecl.isRec)
    env = result.env
  }
  for (const builtin of TREE_NATIVE_BUILTINS) {
    const typeWrapped = buildWrapped(parseLine(builtin.type) as SExpr, env, encType())
    const typeData = unwrapData(typeWrapped)
    env = new Map(env)
    env.set(builtin.name, wrap(builtin.data, typeData))
  }
  return env
}

export function buildNameMap(env: Env): Map<number, string> {
  const map = new Map<number, string>()
  for (const [name, wrapped] of env) {
    const data = unwrapData(wrapped)
    map.set(data.id, name)
  }
  return map
}
