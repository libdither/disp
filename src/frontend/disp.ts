// Default Disp surface frontend: wraps `src/parse.ts` and the Disp-specific
// desugaring/field-resolution helpers behind the syntax-agnostic Frontend contract.

import { parseItems, sumCtorExpr, type Expr, type RecMember } from "../parse.js"
import type { CoreExpr } from "../core/ir.js"
import type { Frontend, CoreItem, CoreGiven, FrontendModule, FrontendEnv, ResolvedRecord, DeclareHookCtx } from "../core/frontend.js"
import { exprToCir, resolveExprRecord, equationLhs } from "../elab/expr.js"
import { binderToPi, binderArity, extractSignature } from "../elab/sugar.js"
import { sugarTree } from "../elab/vocab.js"
import { registerField } from "../elab/fields.js"
import { isGivenHead } from "../elab/modscan.js"
import { coreToTree } from "../elab/cir.js"

type DispFieldMeta = {
  recordField?: { type: string; field: string; fieldType: Expr | null }
}

const isLetHead = (head: Expr | undefined): boolean => head?.tag === "var" && head.name === "let"

function memberToCoreItem(it: RecMember): CoreItem<Expr> {
  switch (it.tag) {
    case "field":
      return {
        tag: "declare",
        name: it.name,
        head: it.head,
        headName: it.head?.tag === "var" ? it.head.name : undefined,
        type: it.type,
        value: it.value,
        docType: it.docType,
        line: it.line,
        endLine: it.endLine,
        meta: it.recordField ? ({ recordField: it.recordField } satisfies DispFieldMeta) : undefined,
      }
    case "test":
      return { tag: "test", lhs: it.lhs, rhs: it.rhs, line: it.line, endLine: it.endLine }
    case "open":
      return { tag: "open", expr: it.expr }
    case "let":
      throw new Error(`internal: lexical let member '${it.name}' at item level`)
    case "bind":
      throw new Error(`internal: monadic bind member '${it.name}' at item level`)
  }
}

export const dispFrontend: Frontend<Expr> = {
  name: "disp",

  parse(src: string, sourcePath?: string): FrontendModule<Expr> {
    const rawItems = parseItems(src, sourcePath)
    const items = rawItems.map(memberToCoreItem)
    const givens: CoreGiven<Expr>[] = []
    let hasExports = false
    for (const it of rawItems) {
      if (it.tag === "field") {
        if (isGivenHead(it.head)) givens.push({ name: it.name, type: it.type, dflt: it.value ?? null })
        else if (!isLetHead(it.head)) hasExports = true
      }
    }
    return { items, givens, hasExports }
  },

  lowerExpr(expr: Expr, env: FrontendEnv): CoreExpr {
    return exprToCir(expr, env.lookup, env.resolveUse, env.sinks)
  },

  lowerType(expr: Expr, env: FrontendEnv): CoreExpr {
    const desugared = sugarTree(env.lookup, "Pi") ? binderToPi(expr) : expr
    return exprToCir(desugared, env.lookup, env.resolveUse, env.sinks)
  },

  shouldCompileTypeAnnotation(typeExpr: Expr, env: FrontendEnv): boolean {
    const hasTypeVocab = Boolean(env.lookup("Type")?.tree || sugarTree(env.lookup, "Pi"))
    return hasTypeVocab && (typeExpr.tag !== "recType" || Boolean(sugarTree(env.lookup, "Telescope")))
  },

  isTypeUniverseAnnotation(typeExpr: Expr, env: FrontendEnv): boolean {
    return typeExpr.tag === "var" && typeExpr.name === "Type" && Boolean(sugarTree(env.lookup, "Pi"))
  },

  preprocessTestLhs(lhs: Expr, env: FrontendEnv): Expr {
    return equationLhs(lhs, env.lookup)
  },

  resolveRecord(expr: Expr, env: FrontendEnv): ResolvedRecord | undefined {
    return resolveExprRecord(expr, env.lookup, env.resolveUse)
  },

  extractParams(valueExpr: Expr, typeExpr: Expr | null | undefined) {
    return extractSignature(valueExpr, typeExpr)
  },

  declaredArity(_valueExpr: Expr | null, typeExpr: Expr | null | undefined, docType?: Expr): number | undefined {
    const ann = typeExpr ?? docType
    return ann != null ? binderArity(ann) : undefined
  },

  openAbstractGiven(openExpr: Expr, givens: CoreGiven<Expr>[]): { name: string; fields: string[] } | undefined {
    if (openExpr.tag !== "var") return undefined
    const spec = givens.find(g => g.name === openExpr.name)
    if (spec?.type?.tag !== "recType") return undefined
    return { name: openExpr.name, fields: spec.type.fields.map(f => f.name) }
  },

  onDeclared(item: Extract<CoreItem<Expr>, { tag: "declare" }>, ctx: DeclareHookCtx<Expr>): void {
    const meta = item.meta as DispFieldMeta | undefined
    if (meta?.recordField) {
      registerField(meta.recordField.type, meta.recordField.field, meta.recordField.fieldType)
    }
    // Block-internal probing (`--print` / `--locals`): synthesize `Parent__local` bindings.
    if (ctx.exposeLocals && ctx.pushed && item.value != null) {
      const prefix: { name: string; val: Expr }[] = []
      let cur: Expr = item.value
      while (cur.tag === "app" && cur.f.tag === "binder" && cur.f.params.length === 1 && cur.f.params[0].name != null && prefix.length < 64) {
        const step = { name: cur.f.params[0].name, val: cur.x }
        let e: Expr = step.val
        for (let i = prefix.length - 1; i >= 0; i--) {
          e = { tag: "app", f: { tag: "binder", fat: true, params: [{ name: prefix[i].name, type: null }], body: e }, x: prefix[i].val }
        }
        try {
          const tree = coreToTree(exprToCir(e, ctx.env.lookup, ctx.env.resolveUse, ctx.env.sinks))
          const local = `${item.name}__${step.name}`
          ctx.define(local, { tree, type: null })
          ctx.emitDef(local, tree, null, null)
        } catch { /* a local that doesn't compile standalone is skipped */ }
        prefix.push(step)
        cur = cur.f.body
      }
    }
    // Constructor auto-declaration for `< Tag : T, ... >` sum literals.
    if (ctx.pushed && item.value != null) {
      let body: Expr = item.value
      while (body.tag === "binder") body = body.body
      if (body.tag === "sumType" && body.variants.length > 0 && ctx.env.lookup("inj")) {
        for (const sv of body.variants) {
          if (ctx.env.lookup(sv.name)) continue
          const cr = ctx.declareValue(sv.name, sumCtorExpr(sv))
          if (cr.pushDef && !cr.priv) ctx.emitDef(sv.name, cr.tree!, cr.type, cr.guard ?? null)
          ctx.recordFieldItem(sv.name, cr.tree ?? undefined)
        }
      }
    }
  },
}
