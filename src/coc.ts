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
import { type Expr, eTree, eFvar, eApp, bracketAbstract, collapse, collapseAndEval } from "./compile.js"
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

export function whnfTree(t: Tree, budget = { remaining: 10000 }): Tree {
  if (budget.remaining <= 0) throw new CocError("WHNF budget exhausted")
  const app = unApp(t)
  if (!app) return t
  budget.remaining--
  const func = whnfTree(app.func, budget)
  const lam = unLam(func)
  if (lam) {
    const result = apply(lam.body, app.arg)
    return whnfTree(result, budget)
  }
  if (treeEqual(func, app.func)) return t
  return encApp(func, app.arg)
}

export function normalize(t: Tree, budget = { remaining: 100000 }): Tree {
  if (budget.remaining <= 0) throw new CocError("Normalization budget exhausted")
  budget.remaining--
  const w = whnfTree(t, budget)
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
  const aN = whnfTree(a, budget)
  const bN = whnfTree(b, budget)
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
      const piTree = whnfTree(expectedType, budget)
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
      const piTree = whnfTree(funcType, budget)
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
  const w = whnfTree(t, budget)
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
function mkTriage(onLeaf: Tree, onStem: Tree, onFork: Tree): Tree { return fork(fork(onLeaf, onStem), onFork) }

function compileTree(params: string[], body: Expr): Tree {
  let e = body
  for (let i = params.length - 1; i >= 0; i--) e = bracketAbstract(params[i], e)
  // Use collapseAndEval (eager apply) instead of collapse (structural treeApply).
  // This is critical for tree-native constants that embed complex tree values via eTree().
  // collapse uses treeApply which for fork(a,b) just builds fork(fork(a,b),g) structurally,
  // but this doesn't behave like apply(fork(a,b),g) at runtime. collapseAndEval eagerly
  // reduces all applications, producing trees that behave correctly when later applied.
  return collapseAndEval(e)
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

// --- Compiled Bool and logic constants ---

const TT: Tree = compileTree(["R", "t", "f"], eFvar("t"))
const FF: Tree = compileTree(["R", "t", "f"], eFvar("f"))
const KFF: Tree = fork(LEAF, FF)        // {_} -> ff  (absorbs 1 arg)
const KKFF: Tree = fork(LEAF, KFF)      // {_ _} -> ff (absorbs 2 args)

// AND : Bool -> Bool -> Bool (eager — reduces to TT/FF structurally)
// and a b = a LEAF b FF
// This forces full evaluation: TT LEAF b FF → b, FF LEAF b FF → FF.
// Unlike the 5-arg version {a b R t f} -> ..., this produces structurally
// identical TT/FF trees, which is critical for tree-native comparisons.
const AND: Tree = compileTree(["a", "b"],
  eApp(eApp(eApp(eFvar("a"), eTree(LEAF)), eFvar("b")), eTree(FF)))

// --- treeEqStep: fuel-based structural tree equality ---
//
// treeEqStep : (Tree -> Tree -> Bool) -> Tree -> Tree -> Bool
//
// Pattern: triage on a, then triage on b in each case.
//   leaf,  leaf       → tt
//   leaf,  _          → ff
//   stem(ac), stem(bc) → self ac bc
//   stem,  _          → ff
//   fork(al,ar), fork(bl,br) → and (self al bl) (self ar br)
//   fork,  _          → ff
//
// Usage: fuel (Tree -> Tree -> Bool) treeEqStep ({x y} -> ff)
// where fuel is a Church numeral controlling recursion depth.

export const TREE_EQ_STEP: Tree = (() => {
  // Case a=leaf: triage on b → leaf:tt, stem(_):ff, fork(_,_):ff
  const aLeafCase = exprTriage(eTree(TT), eTree(KFF), eTree(KKFF))

  // Case a=stem(ac): triage on b → leaf:ff, stem(bc):self ac bc, fork(_,_):ff
  const aStemCase = bracketAbstract("ac",
    exprTriage(
      eTree(FF),
      bracketAbstract("bc", eApp(eApp(eFvar("self"), eFvar("ac")), eFvar("bc"))),
      eTree(KKFF)
    ))

  // Case a=fork(al,ar): triage on b → leaf:ff, stem(_):ff, fork(bl,br):and(self al bl)(self ar br)
  const aForkCase = bracketAbstract("al", bracketAbstract("ar",
    exprTriage(
      eTree(FF),
      eTree(KFF),
      bracketAbstract("bl", bracketAbstract("br",
        eApp(eApp(eTree(AND),
          eApp(eApp(eFvar("self"), eFvar("al")), eFvar("bl"))),
          eApp(eApp(eFvar("self"), eFvar("ar")), eFvar("br")))
      ))
    )))

  // Outer triage on a, then apply result to b
  const body = eApp(eApp(exprTriage(aLeafCase, aStemCase, aForkCase), eFvar("a")), eFvar("b"))
  return compileTree(["self", "a", "b"], body)
})()

// --- S combinator constructor: mkS f g = fork(stem(f), g) ---
const MK_S: Tree = compileTree(["f", "g"],
  eApp(eApp(eTree(LEAF), eApp(eTree(LEAF), eFvar("f"))), eFvar("g")))

// Expr helpers for building combinator trees inside step functions
// mkK(x) = fork(LEAF, x). Built as: apply(stem(LEAF), x).
function exprMkK(x: Expr): Expr { return eApp(eTree(stem(LEAF)), x) }
// mkS(f, g) = fork(stem(f), g). Built as: apply(apply(LEAF, apply(LEAF, f)), g).
function exprMkS(f: Expr, g: Expr): Expr {
  return eApp(eApp(eTree(LEAF), eApp(eTree(LEAF), f)), g)
}

// --- abstractOutStep: fuel-based bracket abstraction ---
//
// abstractOutStep : (Tree -> Tree -> Bool) -> Tree -> (Tree -> Tree) -> Tree -> Tree
//   eq     : equality predicate (treeEq with fixed fuel)
//   target : the subtree to replace
//   self   : recursive abstraction function
//   tree   : the tree to abstract over
//
// Result f satisfies: apply(f, x) = tree[target := x]
//
// Usage: abstractOut fuel eqFuel target tree =
//   fuel (Tree -> Tree) (abstractOutStep (treeEq eqFuel) target) tK tree

export const ABSTRACT_OUT_STEP: Tree = (() => {
  // Check if tree == target
  const isTarget = eApp(eApp(eFvar("eq"), eFvar("tree")), eFvar("target"))

  // K(LEAF) — constant leaf combinator
  const kLeaf = exprMkK(eTree(LEAF))

  // leaf: K(LEAF)
  const leafCase = kLeaf

  // stem(child): S(K(LEAF))(self(child))
  const stemCase = bracketAbstract("child",
    exprMkS(kLeaf, eApp(eFvar("self"), eFvar("child"))))

  // fork(left, right): S(S(K(LEAF))(self(left)))(self(right))
  const forkCase = bracketAbstract("left", bracketAbstract("right",
    exprMkS(
      exprMkS(kLeaf, eApp(eFvar("self"), eFvar("left"))),
      eApp(eFvar("self"), eFvar("right")))))

  // Triage for the "not target" case
  const triageResult = eApp(exprTriage(leafCase, stemCase, forkCase), eFvar("tree"))

  // Branch: isTarget ? I : triageResult
  // Church bool: isTarget LEAF I triageResult (eager, both branches computed)
  const body = eApp(eApp(eApp(isTarget, eTree(LEAF)), eTree(I)), triageResult)

  return compileTree(["eq", "target", "self", "tree"], body)
})()

// --- whnfStep: fuel-based WHNF of encoded CoC terms ---
//
// whnfStep : (Tree -> Tree) -> Tree -> Tree
//   self : recursive WHNF function
//   t    : encoded CoC term
//
// Only App(Lam(...), arg) reduces. Everything else is WHNF.
//
// Usage: whnf fuel t = fuel (Tree -> Tree) whnfStep tI t

export const WHNF_STEP: Tree = (() => {
  // "stuck" = encApp(funcR, appArg) — rebuilt application when func doesn't reduce
  const stuck = eApp(eApp(eTree(ENC_APP_T), eFvar("funcR")), eFvar("appArg"))

  // Inner termCase on funcR (= self(appFunc)):
  //   Type/Var/App/Pi → stuck (can't beta-reduce)
  //   Lam(dom, body) → self(apply(body, appArg))  — beta!
  const innerOnType = stuck
  const innerOnVar = bracketAbstract("_m", stuck)
  const innerOnApp = bracketAbstract("_f", bracketAbstract("_a", stuck))
  const innerOnLam = bracketAbstract("_dom", bracketAbstract("lamBody",
    eApp(eFvar("self"), eApp(eFvar("lamBody"), eFvar("appArg")))))
  const innerOnPi = bracketAbstract("_pd", bracketAbstract("_pb", stuck))

  const innerDispatch = eApp(eApp(eApp(eApp(eApp(eApp(
    eTree(TERM_CASE), innerOnType), innerOnVar), innerOnApp), innerOnLam), innerOnPi),
    eFvar("funcR"))

  // App handler: receives func and arg from outer termCase
  // Let-bind funcR = self(appFunc), then inner dispatch
  const appHandler = bracketAbstract("appFunc", bracketAbstract("appArg",
    eApp(
      bracketAbstract("funcR", innerDispatch),
      eApp(eFvar("self"), eFvar("appFunc"))
    )))

  // Outer termCase: only App does work, rest returns t unchanged
  const onType = eFvar("t")
  const onVar = bracketAbstract("_m", eFvar("t"))
  const onLam = bracketAbstract("_d", bracketAbstract("_b", eFvar("t")))
  const onPi = bracketAbstract("_d", bracketAbstract("_b", eFvar("t")))

  const body = eApp(eApp(eApp(eApp(eApp(eApp(
    eTree(TERM_CASE), onType), onVar), appHandler), onLam), onPi),
    eFvar("t"))

  return compileTree(["self", "t"], body)
})()

// --- convertibleStep: fuel-based convertibility of encoded CoC terms ---
//
// convertibleStep : (Tree -> Tree) -> (Tree -> Tree -> Bool) -> (Tree -> Tree -> Tree -> Bool) -> Tree -> Tree -> Tree -> Bool
//   whnfFn  : WHNF function (with fixed fuel)
//   treeEqF : tree equality (with fixed fuel)
//   self    : recursive convertibility (marker, a, b → Bool)
//   marker  : seed for fresh neutrals under binders
//   a, b    : encoded CoC terms
//
// Usage: convertible fuel whnfFuel eqFuel a b =
//   fuel (Tree -> Tree -> Tree -> Bool)
//     (convertibleStep (whnf whnfFuel) (treeEq eqFuel))
//     ({m x y} -> ff) tDelta a b

export const CONVERTIBLE_STEP: Tree = (() => {
  // Dispatch on bN using inline exprTriage to avoid eta reduction.
  //
  // The encoded term structure is:
  //   Type = LEAF             → triage leaf case
  //   Var(m) = stem(m)        → triage stem case
  //   App/Lam/Pi = fork(tag, payload) → triage fork case, then second triage on tag
  //
  // dispatchOnB builds:
  //   exprTriage(onType, onVarHandler, forkHandler)(bN)
  // where forkHandler triages on the left child to distinguish App/Lam/Pi.
  //
  // This keeps bN inside the triage structure so bracketAbstract("bN", ...)
  // sees bN as free in the func, preventing eta reduction.

  function dispatchOnB(
    onType: Expr,
    onVar: Expr,     // receives marker
    onApp: Expr,     // receives func, arg (from right child)
    onLam: Expr,     // receives domain, body
    onPi: Expr,      // receives domain, body
  ): Expr {
    // Fork case: fork(left, right) → triage on left to distinguish App/Lam/Pi
    // App:  left = LEAF          → onApp(FST(right))(SND(right))
    // Lam:  left = stem(domain)  → onLam(domain)(right)
    // Pi:   left = fork(d, LEAF) → onPi(d)(right)  (ignore second child of left)
    const innerForkTriage = exprTriage(
      // left = LEAF → App case: destructure right into (func, arg)
      eApp(eApp(onApp, eApp(eTree(FST), eFvar("bRight"))), eApp(eTree(SND), eFvar("bRight"))),
      // left = stem(domain) → Lam case
      bracketAbstract("bDom", eApp(eApp(onLam, eFvar("bDom")), eFvar("bRight"))),
      // left = fork(d, _) → Pi case (extract domain from left's left)
      bracketAbstract("bDL", bracketAbstract("_bDR", eApp(eApp(onPi, eFvar("bDL")), eFvar("bRight"))))
    )
    const forkHandler = bracketAbstract("bLeft", bracketAbstract("bRight",
      eApp(innerForkTriage, eFvar("bLeft"))))

    return eApp(exprTriage(onType, onVar, forkHandler), eFvar("bN"))
  }

  // Compare under a binder: apply bodies to encVar(marker), recurse with stem(marker)
  function binderCompare(bodyA: string, bodyB: string): Expr {
    const neutral = eApp(eTree(LEAF), eFvar("marker"))  // stem(marker) = encVar(marker)
    const nextMarker = eApp(eTree(LEAF), eFvar("marker"))
    return eApp(eApp(eApp(eFvar("self"), nextMarker),
      eApp(eFvar(bodyA), neutral)),
      eApp(eFvar(bodyB), neutral))
  }

  // aN=Type: bN must be Type → TT, all other bN tags → FF
  const aTypeCase = dispatchOnB(
    eTree(TT),
    bracketAbstract("_", eTree(FF)),
    bracketAbstract("_", bracketAbstract("_", eTree(FF))),
    bracketAbstract("_", bracketAbstract("_", eTree(FF))),
    bracketAbstract("_", bracketAbstract("_", eTree(FF)))
  )

  // aN=Var(ma): bN must be Var(mb) → treeEq(ma, mb), else FF
  const aVarCase = bracketAbstract("ma", dispatchOnB(
    eTree(FF),
    bracketAbstract("mb", eApp(eApp(eFvar("treeEqF"), eFvar("ma")), eFvar("mb"))),
    bracketAbstract("_", bracketAbstract("_", eTree(FF))),
    bracketAbstract("_", bracketAbstract("_", eTree(FF))),
    bracketAbstract("_", bracketAbstract("_", eTree(FF)))
  ))

  // aN=App(fa,xa): bN must be App(fb,xb) → AND(self(marker,fa,fb))(self(marker,xa,xb))
  const aAppCase = bracketAbstract("fa", bracketAbstract("xa", dispatchOnB(
    eTree(FF),
    bracketAbstract("_", eTree(FF)),
    bracketAbstract("fb", bracketAbstract("xb",
      eApp(eApp(eTree(AND),
        eApp(eApp(eApp(eFvar("self"), eFvar("marker")), eFvar("fa")), eFvar("fb"))),
        eApp(eApp(eApp(eFvar("self"), eFvar("marker")), eFvar("xa")), eFvar("xb"))))),
    bracketAbstract("_", bracketAbstract("_", eTree(FF))),
    bracketAbstract("_", bracketAbstract("_", eTree(FF)))
  )))

  // aN=Lam(da,ba): bN must be Lam(db,bb) → AND(self(marker,da,db))(binderCompare(ba,bb))
  const aLamCase = bracketAbstract("da", bracketAbstract("ba", dispatchOnB(
    eTree(FF),
    bracketAbstract("_", eTree(FF)),
    bracketAbstract("_", bracketAbstract("_", eTree(FF))),
    bracketAbstract("db", bracketAbstract("bb",
      eApp(eApp(eTree(AND),
        eApp(eApp(eApp(eFvar("self"), eFvar("marker")), eFvar("da")), eFvar("db"))),
        binderCompare("ba", "bb")))),
    bracketAbstract("_", bracketAbstract("_", eTree(FF)))
  )))

  // aN=Pi(pda,pba): bN must be Pi(pdb,pbb) → AND(self(marker,pda,pdb))(binderCompare(pba,pbb))
  const aPiCase = bracketAbstract("pda", bracketAbstract("pba", dispatchOnB(
    eTree(FF),
    bracketAbstract("_", eTree(FF)),
    bracketAbstract("_", bracketAbstract("_", eTree(FF))),
    bracketAbstract("_", bracketAbstract("_", eTree(FF))),
    bracketAbstract("pdb", bracketAbstract("pbb",
      eApp(eApp(eTree(AND),
        eApp(eApp(eApp(eFvar("self"), eFvar("marker")), eFvar("pda")), eFvar("pdb"))),
        binderCompare("pba", "pbb"))))
  )))

  // Structural comparison: termCase on aN
  const structuralCompare = eApp(eApp(eApp(eApp(eApp(eApp(
    eTree(TERM_CASE), aTypeCase), aVarCase), aAppCase), aLamCase), aPiCase),
    eFvar("aN"))

  // After WHNF: check treeEq on normalized terms, else structural compare
  const whnfEq = eApp(eApp(eFvar("treeEqF"), eFvar("aN")), eFvar("bN"))
  const afterWhnf = eApp(eApp(eApp(whnfEq, eTree(LEAF)), eTree(TT)), structuralCompare)

  // Bind aN = whnf(a), bN = whnf(b)
  const withBN = eApp(bracketAbstract("bN", afterWhnf), eApp(eFvar("whnfFn"), eFvar("b")))
  const withAN = eApp(bracketAbstract("aN", withBN), eApp(eFvar("whnfFn"), eFvar("a")))

  // Early equality short-circuit
  const earlyEq = eApp(eApp(eFvar("treeEqF"), eFvar("a")), eFvar("b"))
  const body = eApp(eApp(eApp(earlyEq, eTree(LEAF)), eTree(TT)), withAN)

  return compileTree(["whnfFn", "treeEqF", "self", "marker", "a", "b"], body)
})()

// --- TYPECHECK: fold-based CoC type inference step function ---
//
// inferStep : (Tree→Tree) → (Tree→Tree→Tree) → (Tree→Tree→Bool) →
//             (Tree→Tree→Tree) → Tree → Tree → Tree
//   whnfFn  : WHNF function
//   absOutFn: bracket abstraction function
//   eqFn    : tree equality function
//   self    : recursive inference (env → term → wrapped), provided by fold
//   env     : Church-list environment
//   term    : encoded CoC term
//
// Usage: fuel (env→term→wrapped) (inferStep whnf absOut eq) base env term
// where fuel is a Church numeral controlling recursion depth.

export const TYPECHECK: Tree = (() => {
  // --- Expr helpers ---
  let uid = 0
  function fresh() { return `__tc${uid++}` }

  function exprWrap(data: Expr, type: Expr): Expr {
    const v = fresh()
    return bracketAbstract(v, eApp(eApp(eFvar(v), data), type))
  }
  function exprUnwrapData(w: Expr): Expr { return eApp(w, eTree(K_SEL)) }
  function exprUnwrapType(w: Expr): Expr { return eApp(w, eTree(K_STAR_SEL)) }

  function exprEnvLookup(env: Expr, marker: Expr): Expr {
    const entV = fresh(), rstV = fresh()
    const handler = bracketAbstract(entV, bracketAbstract(rstV,
      eApp(eApp(eApp(
        eApp(eApp(eFvar("eq"), eApp(eTree(FST), eFvar(entV))), marker),
        eTree(LEAF)),
        eApp(eTree(SND), eFvar(entV))),
        eFvar(rstV))))
    return eApp(eApp(eApp(env, eTree(LEAF)), eTree(LEAF)), handler)
  }

  function exprEnvCons(entry: Expr, env: Expr): Expr {
    const rV = fresh(), nV = fresh(), cV = fresh()
    return bracketAbstract(rV, bracketAbstract(nV, bracketAbstract(cV,
      eApp(eApp(eFvar(cV), entry),
        eApp(eApp(eApp(env, eFvar(rV)), eFvar(nV)), eFvar(cV))))))
  }

  function exprFreshMarker(term: Expr): Expr {
    return eApp(eApp(eTree(LEAF), term), term) // fork(term, term)
  }
  function exprEncVar(marker: Expr): Expr {
    return eApp(eTree(LEAF), marker) // stem(marker)
  }

  // Pre-computed wrap(Type, Type) as a concrete tree
  const WRAP_TYPE_TYPE: Tree = wrap(encType(), encType())

  // --- Build the body with "self" as a free variable ---
  // self(env, term) is the recursive call (fold handles recursion depth)
  function selfCall(env: Expr, term: Expr): Expr {
    return eApp(eApp(eFvar("self"), env), term)
  }

  // Type case
  const typeCase = eTree(WRAP_TYPE_TYPE)

  // Var(m): envLookup
  const varCase = bracketAbstract("__vm",
    exprEnvLookup(eFvar("env"), eFvar("__vm")))

  // App(func, arg)
  const appCase = (() => {
    const fwV = fresh(), ptV = fresh(), awV = fresh()
    return bracketAbstract("__af", bracketAbstract("__ax",
      eApp(bracketAbstract(fwV,
        eApp(bracketAbstract(ptV,
          eApp(bracketAbstract(awV,
            (() => {
              const argData = exprUnwrapData(eFvar(awV))
              const funcData = exprUnwrapData(eFvar(fwV))
              const piBody = eApp(eTree(SND), eFvar(ptV))
              return exprWrap(
                eApp(eApp(eTree(ENC_APP_T), funcData), argData),
                eApp(piBody, argData))
            })()
          ), selfCall(eFvar("env"), eFvar("__ax")))
        ), eApp(eFvar("whnf"), exprUnwrapType(eFvar(fwV))))
      ), selfCall(eFvar("env"), eFvar("__af")))))
  })()

  // Lam(dom, body)
  const lamCase = (() => {
    const dwV = fresh(), lmV = fresh(), lnV = fresh(), bwV = fresh()
    return bracketAbstract("__ld", bracketAbstract("__lb",
      eApp(bracketAbstract(dwV,
        eApp(bracketAbstract(lmV,
          eApp(bracketAbstract(lnV,
            (() => {
              const domData = exprUnwrapData(eFvar(dwV))
              const entry = eApp(eApp(eTree(LEAF), eFvar(lmV)),
                exprWrap(eFvar(lnV), domData))
              const extEnv = exprEnvCons(entry, eFvar("env"))
              return eApp(bracketAbstract(bwV,
                (() => {
                  const bodyData = exprUnwrapData(eFvar(bwV))
                  const bodyType = exprUnwrapType(eFvar(bwV))
                  const abstBody = eApp(eApp(eFvar("absOut"), eFvar(lnV)), bodyData)
                  const abstType = eApp(eApp(eFvar("absOut"), eFvar(lnV)), bodyType)
                  return exprWrap(
                    eApp(eApp(eTree(ENC_LAM_T), domData), abstBody),
                    eApp(eApp(eTree(ENC_PI_T), domData), abstType))
                })()
              ), selfCall(extEnv, eApp(eFvar("__lb"), eFvar(lnV))))
            })()
          ), exprEncVar(eFvar(lmV)))
        ), exprFreshMarker(eFvar("term")))
      ), selfCall(eFvar("env"), eFvar("__ld")))))
  })()

  // Pi(dom, body)
  const piCase = (() => {
    const dwV = fresh(), pmV = fresh(), pnV = fresh(), cwV = fresh()
    return bracketAbstract("__pd", bracketAbstract("__pb",
      eApp(bracketAbstract(dwV,
        eApp(bracketAbstract(pmV,
          eApp(bracketAbstract(pnV,
            (() => {
              const domData = exprUnwrapData(eFvar(dwV))
              const entry = eApp(eApp(eTree(LEAF), eFvar(pmV)),
                exprWrap(eFvar(pnV), domData))
              const extEnv = exprEnvCons(entry, eFvar("env"))
              return eApp(bracketAbstract(cwV,
                (() => {
                  const codData = exprUnwrapData(eFvar(cwV))
                  const abstCod = eApp(eApp(eFvar("absOut"), eFvar(pnV)), codData)
                  return exprWrap(
                    eApp(eApp(eTree(ENC_PI_T), domData), abstCod),
                    eTree(LEAF))
                })()
              ), selfCall(extEnv, eApp(eFvar("__pb"), eFvar(pnV))))
            })()
          ), exprEncVar(eFvar(pmV)))
        ), exprFreshMarker(eFvar("term")))
      ), selfCall(eFvar("env"), eFvar("__pd")))))
  })()

  // termCase dispatch
  const dispatch = eApp(eApp(eApp(eApp(eApp(eApp(
    eTree(TERM_CASE), typeCase), varCase), appCase), lamCase), piCase),
    eFvar("term"))

  // Fold-based step: [whnf][absOut][eq][self][env][term] → body
  // Usage: fuel (env→term→wrapped) (inferStep whnf absOut eq) base env term
  // where fuel is a Church numeral controlling recursion depth.
  return compileTree(["whnf", "absOut", "eq", "self", "env", "term"], dispatch)
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
  { name: "treeEqStep", type: "(Tree -> Tree -> Bool) -> Tree -> Tree -> Bool",
    data: TREE_EQ_STEP },
  { name: "tI", type: "Tree -> Tree", data: I },
  { name: "tK", type: "Tree -> Tree", data: stem(LEAF) },
  { name: "tS", type: "Tree -> Tree -> Tree", data: MK_S },
  { name: "abstractOutStep",
    type: "(Tree -> Tree -> Bool) -> Tree -> (Tree -> Tree) -> Tree -> Tree",
    data: ABSTRACT_OUT_STEP },
  { name: "whnfStep", type: "(Tree -> Tree) -> Tree -> Tree", data: WHNF_STEP },
  { name: "convertibleStep",
    type: "(Tree -> Tree) -> (Tree -> Tree -> Bool) -> (Tree -> Tree -> Tree -> Bool) -> Tree -> Tree -> Tree -> Bool",
    data: CONVERTIBLE_STEP },
  { name: "inferStep",
    type: "(Tree -> Tree) -> (Tree -> Tree -> Tree) -> (Tree -> Tree -> Bool) -> (Tree -> Tree -> Tree) -> Tree -> Tree -> Tree",
    data: TYPECHECK },
  // Actual tree constructors (not Church-encoded)
  // LEAF is the tree calculus constructor △: apply(LEAF,x)=stem(x), apply(apply(LEAF,x),y)=fork(x,y)
  { name: "tLeaf", type: "Tree", data: LEAF },
  { name: "tStem", type: "Tree -> Tree", data: LEAF },
  { name: "tFork", type: "Tree -> Tree -> Tree", data: LEAF },
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

// Names whose tree data collides with CoC term encodings (e.g. tLeaf/tStem/tFork
// are all LEAF, which is also encType()). Exclude from name map to avoid the
// printer showing "tLeaf" where it should show "Type".
const NAME_MAP_SKIP = new Set(["tLeaf", "tStem", "tFork"])

export function buildNameMap(env: Env): Map<number, string> {
  const map = new Map<number, string>()
  for (const [name, wrapped] of env) {
    if (NAME_MAP_SKIP.has(name)) continue
    const data = unwrapData(wrapped)
    if (!map.has(data.id)) map.set(data.id, name)
  }
  return map
}
