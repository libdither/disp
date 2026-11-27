// Reference alternate frontend: a minimal S-expression syntax for Disp.
// Demonstrates Stage 1 syntax agnosticism — zero dependency on `src/parse.ts`
// or `src/elab/expr.ts`; lowers straight to `CoreExpr` and shares the core
// driver, module cache, cross-syntax `use`, declaration guards, and kernel.
//
// Top-level forms:
//   (open <expr>)
//   (test <lhs> <rhs>)
//   (def <name> [: <type>] <val>)
//   (let <name> [: <type>] <val>)
//   (guard <g> <name> [: <type>] <val>)
//   (given <name> : <type> [<default>])
//
// Expression forms:
//   t | △ | 42 | "str" | ident
//   (lam (x y ...) body)
//   (pi (x A) B) | (arrow A B)
//   (if cond then else)
//   (list e1 e2 ...)
//   (rec (k1 v1) (k2 v2) ...)
//   (use "path" [(k v) ...]) | (use-raw "path" [(k v) ...])
//   (f a b ...)

import type { Tree } from "../eval/eager.js"
import { type CoreExpr, type Span, attachSpan, spanOf, copySpan, formatSpan, coreLit, coreVar, coreApp, coreLam } from "../core/ir.js"
import type { Frontend, CoreItem, CoreGiven, FrontendModule, FrontendEnv, ResolvedRecord } from "../core/frontend.js"
import { elab, B, type ScopeEntry } from "../elab/state.js"
import { stringToTree } from "../elab/literals.js"
import { collectFreeVars, coreToTree } from "../elab/cir.js"
import { sugarTree } from "../elab/vocab.js"

export type Sexp =
  | { tag: "sym"; name: string }
  | { tag: "num"; value: number }
  | { tag: "str"; value: string }
  | { tag: "list"; items: Sexp[] }

function parseSexps(src: string, sourcePath?: string): Sexp[] {
  let i = 0, line = 1, lineStart = 0
  const mkSpan = (sl: number, sc: number, el: number, ec: number): Span =>
    sourcePath ? { line: sl, col: sc, endLine: el, endCol: ec, source: sourcePath } : { line: sl, col: sc, endLine: el, endCol: ec }
  const skipWs = () => {
    while (i < src.length) {
      const c = src[i]
      if (c === "\n") { line++; i++; lineStart = i }
      else if (c === " " || c === "\t" || c === "\r") { i++ }
      else if (c === ";") { while (i < src.length && src[i] !== "\n") i++ }
      else break
    }
  }
  const readOne = (): Sexp => {
    skipWs()
    const sl = line, sc = i - lineStart + 1
    if (i >= src.length) throw new Error(`sexp: unexpected end of input at ${sl}:${sc}`)
    const c = src[i]
    if (c === "(") {
      i++
      const items: Sexp[] = []
      for (;;) {
        skipWs()
        if (i >= src.length) throw new Error(`sexp: unclosed '(' at ${sl}:${sc}`)
        if (src[i] === ")") { i++; return attachSpan({ tag: "list", items }, mkSpan(sl, sc, line, i - lineStart + 1)) }
        items.push(readOne())
      }
    }
    if (c === ")") throw new Error(`sexp: unexpected ')' at ${sl}:${sc}`)
    if (c === '"') {
      const j = src.indexOf('"', i + 1)
      if (j < 0) throw new Error(`sexp: unterminated string at ${sl}:${sc}`)
      for (let k = i + 1; k < j; k++) if (src[k] === "\n") { line++; lineStart = k + 1 }
      const val = src.slice(i + 1, j)
      i = j + 1
      return attachSpan({ tag: "str", value: val }, mkSpan(sl, sc, line, i - lineStart + 1))
    }
    let j = i
    while (j < src.length && !/[\s();"']/.test(src[j])) j++
    const raw = src.slice(i, j)
    i = j
    const sp = mkSpan(sl, sc, line, i - lineStart + 1)
    if (/^[0-9]+$/.test(raw)) return attachSpan({ tag: "num", value: Number(raw) }, sp)
    return attachSpan({ tag: "sym", name: raw }, sp)
  }
  const out: Sexp[] = []
  for (;;) {
    skipWs()
    if (i >= src.length) break
    out.push(readOne())
  }
  return out
}

const sym = (s: Sexp | undefined, ctx: string): string => {
  if (!s || s.tag !== "sym") throw new Error(`sexp ${ctx}: expected symbol`)
  return s.name
}

const atSpan = (s: Sexp, msg: string): string => {
  const sp = spanOf(s)
  return sp ? `${msg} (at ${formatSpan(sp)})` : msg
}

function parseDeclTail(rest: Sexp[], ctx: string): { type: Sexp | null; value: Sexp | null } {
  if (rest.length === 1) return { type: null, value: rest[0] }
  if (rest.length === 2 && rest[0].tag === "sym" && rest[0].name === ":") return { type: rest[1], value: null }
  if (rest.length === 3 && rest[0].tag === "sym" && rest[0].name === ":") return { type: rest[1], value: rest[2] }
  throw new Error(`sexp ${ctx}: expected '<val>' or ': <type> [<val>]'`)
}

function parseFills(pairs: Sexp[], env: FrontendEnv): Map<string, Tree> {
  const fills = new Map<string, Tree>()
  // Allow `(use-raw "path" ())` for an explicit empty fill context (mirrors `use raw "path" {}`).
  if (pairs.length === 1 && pairs[0].tag === "list" && pairs[0].items.length === 0) return fills
  for (const p of pairs) {
    if (p.tag !== "list" || p.items.length !== 2) throw new Error(atSpan(p, "sexp use fill: expected (name expr)"))
    const k = sym(p.items[0], "use fill")
    fills.set(k, coreToTree(lowerSexp(p.items[1], env, new Set())))
  }
  return fills
}

function resolveSexpUse(e: Sexp, env: FrontendEnv): ScopeEntry | undefined {
  if (e.tag !== "list" || e.items.length < 2 || e.items[0].tag !== "sym") return undefined
  const head = e.items[0].name
  if (head !== "use" && head !== "use-raw") return undefined
  const pathNode = e.items[1]
  if (pathNode.tag !== "str") throw new Error(atSpan(pathNode, `sexp ${head}: expected string path`))
  const fills = e.items.length > 2 ? parseFills(e.items.slice(2), env) : undefined
  return env.resolveUse(pathNode.value, head === "use-raw", fills)
}

function lowerSexp(e: Sexp, env: FrontendEnv, bound: Set<string>): CoreExpr {
  const res = lowerSexpInner(e, env, bound)
  return copySpan(e, res)
}

function lowerSexpInner(e: Sexp, env: FrontendEnv, bound: Set<string>): CoreExpr {
  switch (e.tag) {
    case "str": return coreLit(stringToTree(e.value))
    case "num": {
      const zero = sugarTree(env.lookup, "zero")
      const succ = sugarTree(env.lookup, "succ")
      if (!zero || !succ) throw new Error(atSpan(e, `numeric literal ${e.value}: zero and succ must be in scope`))
      let cur = zero
      for (let k = 0; k < e.value; k++) cur = elab.cs.apply(succ, cur, B())
      return coreLit(cur)
    }
    case "sym": {
      if (e.name === "t" || e.name === "△") return coreLit(elab.cs.leaf())
      if (!bound.has(e.name)) {
        const entry = env.lookup(e.name)
        if (entry?.tree != null) return coreLit(entry.tree)
      }
      return coreVar(e.name)
    }
    case "list": {
      if (e.items.length === 0) return coreLit(elab.cs.leaf())
      const [hd, ...args] = e.items
      if (hd.tag === "sym") {
        if (hd.name === "lam") {
          if (args.length !== 2 || args[0].tag !== "list") throw new Error(atSpan(e, "sexp lam: expected (lam (x ...) body)"))
          const params = args[0].items.map(p => sym(p, "lam param"))
          const innerBound = new Set(bound)
          for (const p of params) innerBound.add(p)
          let body = lowerSexp(args[1], env, innerBound)
          for (let k = params.length - 1; k >= 0; k--) body = coreLam(params[k], body)
          return body
        }
        if (hd.name === "pi") {
          if (args.length !== 2 || args[0].tag !== "list" || args[0].items.length !== 2)
            throw new Error(atSpan(e, "sexp pi: expected (pi (x A) B)"))
          const piTree = sugarTree(env.lookup, "Pi")
          if (!piTree) throw new Error(atSpan(e, "sexp pi: 'Pi' must be in scope"))
          const pname = sym(args[0].items[0], "pi param")
          const dom = lowerSexp(args[0].items[1], env, bound)
          const innerBound = new Set(bound); innerBound.add(pname)
          const cod = coreLam(pname, lowerSexp(args[1], env, innerBound))
          return coreApp(coreApp(coreLit(piTree), dom), cod)
        }
        if (hd.name === "arrow") {
          if (args.length < 2) throw new Error(atSpan(e, "sexp arrow: expected (arrow A B ...)"))
          const piTree = sugarTree(env.lookup, "Pi")
          if (!piTree) throw new Error(atSpan(e, "sexp arrow: 'Pi' must be in scope"))
          let cur = lowerSexp(args[args.length - 1], env, bound)
          for (let k = args.length - 2; k >= 0; k--) {
            const dom = lowerSexp(args[k], env, bound)
            cur = coreApp(coreApp(coreLit(piTree), dom), coreLam("_", cur))
          }
          return cur
        }
        if (hd.name === "list") {
          let cur: CoreExpr = coreLit(elab.cs.leaf())
          for (let k = args.length - 1; k >= 0; k--) {
            cur = coreApp(coreApp(coreLit(elab.cs.leaf()), lowerSexp(args[k], env, bound)), cur)
          }
          return cur
        }
        if (hd.name === "rec") {
          const mkr = sugarTree(env.lookup, "make_record")
          const lc = sugarTree(env.lookup, "list_const")
          if (!mkr || !lc) throw new Error(atSpan(e, "sexp rec: 'make_record' and 'list_const' must be in scope"))
          const leaf = coreLit(elab.cs.leaf())
          let names: Tree = elab.cs.leaf()
          let vals: CoreExpr = leaf
          for (let k = args.length - 1; k >= 0; k--) {
            const pair = args[k]
            if (pair.tag !== "list" || pair.items.length !== 2) throw new Error(atSpan(pair, "sexp rec: expected (field expr)"))
            names = elab.cs.fork(stringToTree(sym(pair.items[0], "rec field")), names)
            vals = coreApp(coreApp(leaf, coreApp(coreLit(lc), lowerSexp(pair.items[1], env, bound))), vals)
          }
          return coreApp(coreApp(coreLit(mkr), coreLit(names)), vals)
        }
        if (hd.name === "if") {
          if (args.length !== 3) throw new Error(atSpan(e, "sexp if: expected (if cond then else)"))
          const condTree = sugarTree(env.lookup, "cond")
          if (!condTree) throw new Error(atSpan(e, "sexp if: 'cond' must be in scope"))
          const c = lowerSexp(args[0], env, bound)
          const th = lowerSexp(args[1], env, bound)
          const el = lowerSexp(args[2], env, bound)
          const fvs: string[] = []
          const seen = new Set<string>()
          collectFreeVars(th, new Set(), fvs, seen)
          collectFreeVars(el, new Set(), fvs, seen)
          const wrap = (b: CoreExpr) => {
            let r = b
            for (let k = fvs.length - 1; k >= 0; k--) r = coreLam(fvs[k], r)
            return r
          }
          let out: CoreExpr = coreApp(coreApp(coreApp(coreLit(condTree), c), wrap(th)), wrap(el))
          for (const v of fvs) out = coreApp(out, coreVar(v))
          return out
        }
        if (hd.name === "use" || hd.name === "use-raw") {
          const entry = resolveSexpUse(e, env)!
          return coreLit(entry.tree!)
        }
      }
      let cur = lowerSexp(hd, env, bound)
      for (const a of args) cur = coreApp(cur, lowerSexp(a, env, bound))
      return cur
    }
  }
}

export const sexpFrontend: Frontend<Sexp> = {
  name: "sexp",

  parse(src: string, sourcePath?: string): FrontendModule<Sexp> {
    const forms = parseSexps(src, sourcePath)
    const items: CoreItem<Sexp>[] = []
    const givens: CoreGiven<Sexp>[] = []
    let hasExports = false
    for (const f of forms) {
      if (f.tag !== "list" || f.items.length < 2) throw new Error(atSpan(f, "sexp: expected top-level form"))
      const kw = sym(f.items[0], "top-level form")
      const sp = spanOf(f)
      if (kw === "open") {
        if (f.items.length !== 2) throw new Error(atSpan(f, "sexp open: expected (open expr)"))
        items.push({ tag: "open", expr: f.items[1] })
      } else if (kw === "test") {
        if (f.items.length !== 3) throw new Error(atSpan(f, "sexp test: expected (test lhs rhs)"))
        items.push({ tag: "test", lhs: f.items[1], rhs: f.items[2], line: sp?.line, endLine: sp?.endLine })
      } else if (kw === "def") {
        const name = sym(f.items[1], "def name")
        const { type, value } = parseDeclTail(f.items.slice(2), "def")
        hasExports = true
        items.push({ tag: "declare", name, type, value, line: sp?.line, endLine: sp?.endLine })
      } else if (kw === "let") {
        const name = sym(f.items[1], "let name")
        const { type, value } = parseDeclTail(f.items.slice(2), "let")
        items.push({
          tag: "declare", name, head: { tag: "sym", name: "let" }, headName: "let",
          type, value, line: sp?.line, endLine: sp?.endLine,
        })
      } else if (kw === "guard") {
        if (f.items.length < 4) throw new Error(atSpan(f, "sexp guard: expected (guard g name [: T] val)"))
        const g = f.items[1]
        const name = sym(f.items[2], "guard name")
        const { type, value } = parseDeclTail(f.items.slice(3), "guard")
        hasExports = true
        items.push({
          tag: "declare", name, head: { tag: "list", items: [{ tag: "sym", name: "guard" }, g] },
          type, value, line: sp?.line, endLine: sp?.endLine,
        })
      } else if (kw === "given") {
        const name = sym(f.items[1], "given name")
        const { type, value } = parseDeclTail(f.items.slice(2), "given")
        givens.push({ name, type, dflt: value })
        items.push({
          tag: "declare", name, head: { tag: "sym", name: "given" }, headName: "given",
          type, value, line: sp?.line, endLine: sp?.endLine,
        })
      } else {
        throw new Error(atSpan(f, `sexp: unknown top-level form '${kw}'`))
      }
    }
    return { items, givens, hasExports }
  },

  lowerExpr(expr: Sexp, env: FrontendEnv): CoreExpr {
    return lowerSexp(expr, env, new Set())
  },

  resolveRecord(expr: Sexp, env: FrontendEnv): ResolvedRecord | undefined {
    if (expr.tag === "sym") {
      const e = env.lookup(expr.name)
      return e?.fields ? { fields: e.fields, fieldTrees: e.fieldTrees, fieldTypes: e.fieldTypes, fieldGuards: e.fieldGuards, fieldCerts: e.fieldCerts, fieldParams: e.fieldParams } : undefined
    }
    const used = resolveSexpUse(expr, env)
    return used?.fields ? { fields: used.fields, fieldTrees: used.fieldTrees, fieldTypes: used.fieldTypes, fieldGuards: used.fieldGuards, fieldCerts: used.fieldCerts, fieldParams: used.fieldParams } : undefined
  },
}
