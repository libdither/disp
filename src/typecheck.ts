// Bidirectional type checker for the Calculus of Constructions (CoC).
// Works on the surface AST (SExpr) with named variables.
// Type:Type (no universe hierarchy).

import { type SExpr, svar, sapp, slam, spi, stype } from "./parse.js"

// --- Type errors ---

export class TypeError extends Error {
  constructor(msg: string) { super(msg) }
}

// --- Context ---
// Stores both type and optional definition (for let-bound names).

export type CtxEntry = { name: string, type: SExpr, value?: SExpr }
export type Context = CtxEntry[]

function lookupCtx(ctx: Context, name: string): CtxEntry | null {
  for (let i = ctx.length - 1; i >= 0; i--) {
    if (ctx[i].name === name) return ctx[i]
  }
  return null
}

function extendCtx(ctx: Context, name: string, type: SExpr, value?: SExpr): Context {
  return [...ctx, { name, type, value }]
}

// --- Named substitution ---

let freshCounter = 0
function freshName(base: string): string {
  return `${base}$${freshCounter++}`
}

export function substNamed(name: string, body: SExpr, val: SExpr): SExpr {
  switch (body.tag) {
    case "svar":
      return body.name === name ? val : body
    case "stype":
      return body
    case "sapp":
      return sapp(substNamed(name, body.func, val), substNamed(name, body.arg, val))
    case "slam": {
      if (body.params.includes(name)) return body // shadowed
      const freeInVal = freeVars(val)
      const newParams: string[] = []
      let currentBody = body.body
      for (const p of body.params) {
        if (freeInVal.has(p)) {
          const fresh = freshName(p)
          currentBody = substNamed(p, currentBody, svar(fresh))
          newParams.push(fresh)
        } else {
          newParams.push(p)
        }
      }
      return slam(newParams, substNamed(name, currentBody, val))
    }
    case "spi": {
      const domain = substNamed(name, body.domain, val)
      if (body.name === name) {
        // Shadowed in codomain — don't substitute there
        return spi(body.name, domain, body.codomain)
      }
      const freeInVal = freeVars(val)
      if (freeInVal.has(body.name)) {
        const fresh = freshName(body.name)
        const renamedCod = substNamed(body.name, body.codomain, svar(fresh))
        return spi(fresh, domain, substNamed(name, renamedCod, val))
      }
      return spi(body.name, domain, substNamed(name, body.codomain, val))
    }
  }
}

function freeVars(expr: SExpr): Set<string> {
  const vars = new Set<string>()
  collectFreeVars(expr, new Set(), vars)
  return vars
}

function collectFreeVars(expr: SExpr, bound: Set<string>, free: Set<string>): void {
  switch (expr.tag) {
    case "svar":
      if (!bound.has(expr.name)) free.add(expr.name)
      break
    case "stype": break
    case "sapp":
      collectFreeVars(expr.func, bound, free)
      collectFreeVars(expr.arg, bound, free)
      break
    case "slam": {
      const newBound = new Set(bound)
      for (const p of expr.params) newBound.add(p)
      collectFreeVars(expr.body, newBound, free)
      break
    }
    case "spi": {
      collectFreeVars(expr.domain, bound, free)
      const newBound = new Set(bound)
      if (expr.name !== "_") newBound.add(expr.name)
      collectFreeVars(expr.codomain, newBound, free)
      break
    }
  }
}

// --- WHNF on SExpr (context-aware: unfolds definitions) ---

export function whnfSExpr(term: SExpr, ctx: Context = [], budget = { remaining: 10000 }): SExpr {
  if (budget.remaining <= 0) throw new TypeError("WHNF budget exhausted")

  switch (term.tag) {
    case "svar": {
      // Unfold definitions
      const entry = lookupCtx(ctx, term.name)
      if (entry?.value) {
        budget.remaining--
        return whnfSExpr(entry.value, ctx, budget)
      }
      return term
    }
    case "sapp": {
      budget.remaining--
      const func = whnfSExpr(term.func, ctx, budget)
      if (func.tag === "slam") {
        const [param, ...rest] = func.params
        const substituted = substNamed(param, func.body, term.arg)
        if (rest.length > 0) {
          return whnfSExpr(slam(rest, substituted), ctx, budget)
        }
        return whnfSExpr(substituted, ctx, budget)
      }
      return sapp(func, term.arg)
    }
    case "slam": {
      if (term.params.length > 1) {
        const [first, ...rest] = term.params
        return slam([first], slam(rest, term.body))
      }
      return term
    }
    default:
      return term
  }
}

// --- Convertibility on SExpr ---

export function convertibleSExpr(a: SExpr, b: SExpr, ctx: Context = [], budget = { remaining: 10000 }): boolean {
  const aN = whnfSExpr(a, ctx, budget)
  const bN = whnfSExpr(b, ctx, budget)
  return sexprAlphaEqual(aN, bN, ctx)
}

function sexprAlphaEqual(a: SExpr, b: SExpr, ctx: Context): boolean {
  if (a.tag !== b.tag) return false

  switch (a.tag) {
    case "svar":
      return a.name === (b as typeof a).name
    case "stype":
      return true
    case "sapp": {
      const bApp = b as typeof a
      return sexprAlphaEqual(a.func, bApp.func, ctx) &&
             sexprAlphaEqual(a.arg, bApp.arg, ctx)
    }
    case "slam": {
      const bLam = b as typeof a
      if (a.params.length !== bLam.params.length) return false
      let aBody = a.body
      let bBody = bLam.body
      for (let i = 0; i < a.params.length; i++) {
        const fresh = freshName("eq")
        aBody = substNamed(a.params[i], aBody, svar(fresh))
        bBody = substNamed(bLam.params[i], bBody, svar(fresh))
      }
      return sexprAlphaEqual(aBody, bBody, ctx)
    }
    case "spi": {
      const bPi = b as typeof a
      if (!sexprAlphaEqual(a.domain, bPi.domain, ctx)) return false
      const fresh = freshName("eq")
      const aCod = a.name === "_" ? a.codomain : substNamed(a.name, a.codomain, svar(fresh))
      const bCod = bPi.name === "_" ? bPi.codomain : substNamed(bPi.name, bPi.codomain, svar(fresh))
      return sexprAlphaEqual(aCod, bCod, ctx)
    }
  }
  return false
}

// --- Type checker ---

export function infer(ctx: Context, term: SExpr): SExpr {
  switch (term.tag) {
    case "stype":
      return stype

    case "svar": {
      const entry = lookupCtx(ctx, term.name)
      if (!entry) throw new TypeError(`Unbound variable: ${term.name}`)
      return entry.type
    }

    case "spi": {
      const domType = infer(ctx, term.domain)
      ensureSort(domType, ctx, `Domain of Pi type`)
      const extCtx = term.name === "_" ? ctx : extendCtx(ctx, term.name, term.domain)
      const codType = infer(extCtx, term.codomain)
      ensureSort(codType, ctx, `Codomain of Pi type`)
      return codType
    }

    case "slam":
      throw new TypeError(`Cannot infer type of lambda expression. Use a type annotation.`)

    case "sapp": {
      const funcType = whnfSExpr(infer(ctx, term.func), ctx)

      if (funcType.tag !== "spi") {
        throw new TypeError(`Expected function type, got: ${printType(funcType)}`)
      }

      check(ctx, term.arg, funcType.domain)

      if (funcType.name === "_") return funcType.codomain
      return substNamed(funcType.name, funcType.codomain, term.arg)
    }
  }
}

export function check(ctx: Context, term: SExpr, expected: SExpr): void {
  if (term.tag === "slam") {
    const piType = whnfSExpr(expected, ctx)
    if (piType.tag !== "spi") {
      throw new TypeError(`Lambda expression needs function type, got: ${printType(expected)}`)
    }

    const [param, ...restParams] = term.params
    const innerBody = restParams.length > 0 ? slam(restParams, term.body) : term.body

    const extCtx = extendCtx(ctx, param, piType.domain)

    let codomain = piType.codomain
    if (piType.name !== "_" && piType.name !== param) {
      codomain = substNamed(piType.name, codomain, svar(param))
    }

    check(extCtx, innerBody, codomain)
    return
  }

  const inferred = infer(ctx, term)
  if (!convertibleSExpr(inferred, expected, ctx)) {
    throw new TypeError(`Type mismatch: expected ${printType(expected)}, got ${printType(inferred)}`)
  }
}

function ensureSort(type: SExpr, ctx: Context, context: string): void {
  const nf = whnfSExpr(type, ctx)
  if (nf.tag !== "stype") {
    throw new TypeError(`${context} must be a type, got: ${printType(type)}`)
  }
}

function printType(expr: SExpr): string {
  switch (expr.tag) {
    case "svar": return expr.name
    case "stype": return "Type"
    case "sapp": return `(${printType(expr.func)} ${printType(expr.arg)})`
    case "slam": return `{${expr.params.join(" ")}} -> ${printType(expr.body)}`
    case "spi":
      if (expr.name === "_") return `(${printType(expr.domain)} -> ${printType(expr.codomain)})`
      return `((${expr.name} : ${printType(expr.domain)}) -> ${printType(expr.codomain)})`
  }
}

// --- Top-level: type-check a declaration ---

export function checkDecl(
  ctx: Context,
  name: string,
  type: SExpr | null,
  value: SExpr,
  isRec = false
): { ctx: Context, type: SExpr } {
  if (isRec) {
    if (type === null) {
      throw new TypeError(`Recursive definition '${name}' requires a type annotation`)
    }
    const typeOfType = infer(ctx, type)
    ensureSort(typeOfType, ctx, `Type annotation for ${name}`)
    // Pre-extend context with name:type but NO value (prevents infinite unfolding)
    const preCtx = extendCtx(ctx, name, type)
    check(preCtx, value, type)
    // Return context with the value so it can be unfolded in later definitions
    return { ctx: extendCtx(ctx, name, type, value), type }
  }

  if (type !== null) {
    const typeOfType = infer(ctx, type)
    ensureSort(typeOfType, ctx, `Type annotation for ${name}`)
    check(ctx, value, type)
    // Store the value as a definition so it can be unfolded
    return { ctx: extendCtx(ctx, name, type, value), type }
  } else {
    const inferred = infer(ctx, value)
    return { ctx: extendCtx(ctx, name, inferred, value), type: inferred }
  }
}
