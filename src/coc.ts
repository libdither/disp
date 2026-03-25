// Calculus of Constructions encoded directly in tree calculus.
//
// Terms are encoded as trees:
//   Type         = LEAF
//   Var(marker)  = stem(marker)
//   App(M, N)    = fork(LEAF, fork(M, N))
//   Lam(A, body) = fork(stem(A), body)        -- body is bracket-abstracted
//   Pi(A, body)  = fork(fork(A, LEAF), body)   -- body is bracket-abstracted
//
// Each value is "wrapped" as a fork pair carrying (data, type):
//   wrap(data, type) = fork(data, type)
//   unwrapData(w)    = w.left
//   unwrapType(w)    = w.right

import { type Tree, LEAF, stem, fork, treeEqual, apply, I, treeApply, BudgetExhausted } from "./tree.js"
import { type Expr, eTree, eFvar, eApp, bracketAbstract, collapse } from "./compile.js"
import { type SExpr } from "./parse.js"

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

// --- WRAP (fork pair) ---

export function wrap(data: Tree, type: Tree): Tree {
  return fork(data, type)
}

export function unwrapData(wrapped: Tree): Tree {
  if (wrapped.tag !== "fork") throw new CocError("Expected wrapped value (fork pair)")
  return wrapped.left
}

export function unwrapType(wrapped: Tree): Tree {
  if (wrapped.tag !== "fork") throw new CocError("Expected wrapped value (fork pair)")
  return wrapped.right
}

// --- Primitive type constants ---
// Must not collide with encVar(freshMarker()). Fresh markers are fork(LEAF, stem^n(LEAF)),
// so encVar(marker) = stem(fork(LEAF, ...)). We use fork(fork(LEAF,LEAF), fork(...)) patterns
// which are Pi-tagged but that's fine since they're only used in TYPE positions.

export const TREE_TYPE = fork(fork(LEAF, LEAF), fork(LEAF, LEAF))
export const BOOL_TYPE = fork(fork(LEAF, LEAF), fork(LEAF, stem(LEAF)))
export const NAT_TYPE  = fork(fork(LEAF, LEAF), fork(stem(LEAF), LEAF))

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
// Native builtin tracking (for delta reduction in convertibility)
// ============================================================

// Native builtins are tree constants (like BOOL_ELIM_DATA, NAT_ELIM_DATA) whose
// internal tree structure may accidentally match the CoC encoding patterns (encApp,
// encLam, etc.). We track their IDs so evalToNative can avoid decomposing them.
// For some trees (like recursive definitions), the encoded form (omega combinator)
// differs from the compiled form (FIX-based). We map encoded IDs to their compiled
// forms so evalToNative uses the version that works with eager apply().
const nativeBuiltinIds = new Set<number>()
const nativeCompiledForms = new Map<number, Tree>()

export function registerNativeBuiltinId(id: number, compiledForm?: Tree): void {
  nativeBuiltinIds.add(id)
  if (compiledForm) {
    nativeCompiledForms.set(id, compiledForm)
    nativeBuiltinIds.add(compiledForm.id)  // also protect the compiled form from decomposition
  }
}

export function clearNativeBuiltins(): void {
  nativeBuiltinIds.clear()
  nativeCompiledForms.clear()
  whnfMemo.clear()
}

function isNativeBuiltin(t: Tree): boolean {
  return nativeBuiltinIds.has(t.id)
}

function getNativeForm(t: Tree): Tree {
  return nativeCompiledForms.get(t.id) ?? t
}

// Check if a tree looks like encVar(freshMarker()) — i.e., a free variable.
// Fresh markers are fork(LEAF, stem^n(LEAF)), so encVar(marker) = stem(fork(LEAF, stem^n(LEAF))).
function isEncVarMarker(t: Tree): boolean {
  if (t.tag !== "stem") return false
  const child = t.child
  if (child.tag !== "fork" || child.left.tag !== "leaf") return false
  let r = child.right
  while (r.tag === "stem") r = r.child
  return r.tag === "leaf"
}

// Evaluate a closed encoded term to its native tree form using tree calculus apply().
// Returns null if the term contains free variables (markers) or exceeds budget.
// This bridges the gap between the encoded CoC world and the native tree evaluator:
// encoded applications of builtins (like boolElim, natElim) get evaluated via apply().
export function evalToNative(t: Tree, budget = { remaining: 100000 }): Tree | null {
  if (budget.remaining <= 0) return null
  budget.remaining--

  // Don't decompose registered native builtins — their tree structure may
  // accidentally match encoding patterns (e.g., fork(LEAF, fork(X,Y)) looks like encApp).
  // Use the compiled form if available (e.g., FIX-based for recursive defs).
  if (isNativeBuiltin(t)) return getNativeForm(t)

  const app = unApp(t)
  if (!app) {
    // Not an encoded application. Check for free variables.
    if (isEncVarMarker(t)) return null
    return t
  }

  // It's encApp(func, arg) — evaluate both sides
  const funcEvaled = evalToNative(app.func, budget)
  if (funcEvaled === null) return null

  const argEvaled = evalToNative(app.arg, budget)
  if (argEvaled === null) return null

  // If func is a genuine encoded lambda (not a native tree whose structure
  // accidentally matches encLam), beta-reduce via apply(body, arg)
  if (!isNativeBuiltin(funcEvaled)) {
    const lam = unLam(funcEvaled)
    if (lam) {
      try {
        const result = apply(lam.body, argEvaled, budget)
        return evalToNative(result, budget)
      } catch (e) {
        if (e instanceof BudgetExhausted) return null
        throw e
      }
    }
  }

  // func is a native tree (compiled builtin, partial application, etc.) — apply natively.
  // Register the result so future evalToNative calls won't decompose it either.
  try {
    const result = apply(funcEvaled, argEvaled, budget)
    nativeBuiltinIds.add(result.id)
    return result
  } catch (e) {
    if (e instanceof BudgetExhausted) return null
    throw e
  }
}

// ============================================================
// Phase 3: WHNF, Convertibility, and Core Building
// ============================================================

export class CocError extends Error {
  constructor(msg: string) { super(msg) }
}

// WHNF memoization: whnfTree is deterministic and idempotent, so we cache results.
const whnfMemo = new Map<number, Tree>()

export function clearWhnfCache(): void {
  whnfMemo.clear()
}

export function whnfTree(t: Tree, budget = { remaining: 10000 }): Tree {
  const cached = whnfMemo.get(t.id)
  if (cached !== undefined) return cached
  if (budget.remaining <= 0) throw new CocError("WHNF budget exhausted")
  // Native builtins are opaque values — don't decompose their internal tree structure
  if (isNativeBuiltin(t)) return t
  const app = unApp(t)
  if (!app) return t
  budget.remaining--
  const func = whnfTree(app.func, budget)
  // Don't treat native builtins as encoded lambdas (S combinators look like encLam)
  const lam = isNativeBuiltin(func) ? null : unLam(func)
  if (lam) {
    const result = apply(lam.body, app.arg)
    const whnf = whnfTree(result, budget)
    whnfMemo.set(t.id, whnf)
    return whnf
  }
  const out = treeEqual(func, app.func) ? t : encApp(func, app.arg)
  whnfMemo.set(t.id, out)
  return out
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
  if (treeEqual(aN, b)) return true
  const bN = whnfTree(b, budget)
  if (treeEqual(aN, bN)) return true
  const aTag = termTag(aN)
  const bTag = termTag(bN)
  if (aTag === bTag) {
    switch (aTag) {
      case "type": return true
      case "var": return treeEqual(unVar(aN)!, unVar(bN)!)
      case "app": {
        const aApp = unApp(aN)!
        const bApp = unApp(bN)!
        if (convertible(aApp.func, bApp.func, budget) &&
            convertible(aApp.arg, bApp.arg, budget)) return true
        break
      }
      case "lam": {
        const aLam = unLam(aN)!
        const bLam = unLam(bN)!
        if (convertible(aLam.domain, bLam.domain, budget) &&
            convertibleUnderBinder(aLam.body, bLam.body, budget)) return true
        break
      }
      case "pi": {
        const aPi = unPi(aN)!
        const bPi = unPi(bN)!
        if (convertible(aPi.domain, bPi.domain, budget) &&
            convertibleUnderBinder(aPi.body, bPi.body, budget)) return true
        break
      }
    }
  }
  // Fallback: try native evaluation for closed terms (delta reduction).
  // This handles cases where builtins (boolElim, natElim, etc.) are stuck
  // in the encoded world but can be reduced by the tree calculus evaluator.
  const aEval = evalToNative(aN)
  const bEval = evalToNative(bN)
  if (aEval !== null && bEval !== null && treeEqual(aEval, bEval)) return true
  return false
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
    case "stree":
      return wrap(TREE_TYPE, encType())
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
  // Register as native builtin so evalToNative won't incorrectly decompose it
  nativeBuiltinIds.add(result.id)
  const newEnv = new Map(env)
  newEnv.set(name, wrap(result, typeData))
  return { env: newEnv, type: typeData }
}

// Re-export printer from dedicated module for backward compatibility
export { printEncoded } from "./coc-printer.js"

