// Bracket abstraction + driver. Compiles parsed AST (from parse.ts)
// to tree calculus via bracket abstraction, resolves scopes, inlines
// `use`d files, and produces Decl[].
//
// Sections:
//   5. Bracket abstraction (Expr → Cir → Tree)
//   6. Driver (scope stack, `use`, produces Decl[])

import { readFileSync } from "node:fs"
import { dirname, resolve as pathResolve } from "node:path"
import {
  Tree, LEAF, stem, fork, applyTree, treeEqual, prettyTree, getApplyStats, setTreeEqId, getTreeEqId, type ApplyStats,
  SCOTT_TT,
} from "./tree.js"
import {
  parseItems,
  type Expr, type Param, type TypedField, type NamedField, type RecMember, type Tok,
} from "./parse.js"

// ──────────────────────── 5. Bracket abstraction ─────────────────────────

// Step budget passed to applyTree from the compiler/elaborator. Large
// enough that elaboration of any well-formed program terminates; small
// enough that runaway evaluation aborts before exhausting host memory.
const APPLY_BUDGET = 10_000_000

// CIR: intermediate representation with explicit S/K/I sentinels.
type Cir =
  | { tag: "lit"; t: Tree }
  | { tag: "var"; name: string }
  | { tag: "app"; f: Cir; x: Cir }
  | { tag: "lam"; x: string; body: Cir }
  | { tag: "S" } | { tag: "K" } | { tag: "I" }

const S: Cir = { tag: "S" }
const K: Cir = { tag: "K" }
const I_CIR: Cir = { tag: "I" }
const cap = (f: Cir, x: Cir): Cir => ({ tag: "app", f, x })

const I_TREE = fork(fork(LEAF, LEAF), LEAF)
const K_TREE = stem(LEAF)
const S_TREE = fork(stem(fork(LEAF, LEAF)), LEAF)

function containsFree(e: Cir, name: string): boolean {
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
function collectFreeVars(e: Cir, bound: Set<string>, out: string[], seen: Set<string>): void {
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

function eliminateLams(e: Cir): Cir {
  switch (e.tag) {
    case "lit": case "var": case "S": case "K": case "I": return e
    case "app": return cap(eliminateLams(e.f), eliminateLams(e.x))
    case "lam": return abstractName(e.x, eliminateLams(e.body))
  }
}

function cirToTree(e: Cir): Tree {
  switch (e.tag) {
    case "lit": return e.t
    case "var": throw new Error(`cirToTree: unresolved free variable ${e.name}`)
    case "I":   return I_TREE
    case "K":   return K_TREE
    case "S":   return S_TREE
    case "lam": throw new Error(`cirToTree: unexpected lambda for ${e.x}`)
    case "app": {
      if (e.f.tag === "app" && e.f.f.tag === "S") return fork(stem(cirToTree(e.f.x)), cirToTree(e.x))
      // Full K application: K(x)(y) → x (drop second arg)
      if (e.f.tag === "app" && e.f.f.tag === "K") return cirToTree(e.f.x)
      if (e.f.tag === "K") return fork(LEAF, cirToTree(e.x))
      if (e.f.tag === "I") return cirToTree(e.x)
      // Partial S application: S(x) → stem(stem(x)) so that S(x)(y) = fork(stem(x), y)
      if (e.f.tag === "S") return stem(stem(cirToTree(e.x)))
      return applyTree(cirToTree(e.f), cirToTree(e.x), APPLY_BUDGET)
    }
  }
}

// Build selector: \x0 x1 ... xn-1. xi (picks the i-th of n arguments)
function buildSelector(n: number, i: number): Cir {
  const names = Array.from({ length: n }, (_, j) => `__sel${j}`)
  let body: Cir = { tag: "var", name: names[i] }
  for (let j = n - 1; j >= 0; j--) {
    body = { tag: "lam", x: names[j], body }
  }
  return body
}

function selectorTree(n: number, i: number): Tree {
  return cirToTree(eliminateLams(buildSelector(n, i)))
}

// Scope entry: a compiled tree plus optional compile-time record metadata.
// Field names/trees are parser metadata only; runtime records remain ordinary
// Church-encoded values.
interface ScopeEntry {
  tree?: Tree
  type?: Tree | null    // null = untyped, undefined = not yet set
  fields?: string[]
  fieldTrees?: Tree[]
  fieldTypes?: (Tree | null)[]  // per-field types for open
  fieldInnerFields?: (string[] | undefined)[]
  fieldInnerTrees?: (Tree[] | undefined)[]
}

// Detect `{_} -> body` / `{x} -> body` thunks where the parameter is unused.
// Returns the thunk body if `e` is a single-param binder whose param is not
// referenced; null otherwise. Used by the select_lazy → match rewrite.
function asUnusedParamThunk(e: Expr): Expr | null {
  if (e.tag !== "binder" || e.params.length !== 1) return null
  const p = e.params[0]
  const name = p.name
  // Anonymous binder (name=null, written as `{_}`) always qualifies.
  if (name === null) return e.body
  // Named binder qualifies if the body doesn't mention the name.
  if (!exprMentions(e.body, name)) return e.body
  return null
}

// Free-variable check on the Expr AST. Conservative: returns true if the
// name could be referenced anywhere in `e` (modulo binder shadowing).
function exprMentions(e: Expr, name: string): boolean {
  switch (e.tag) {
    case "leaf": case "num": case "str": case "hole": case "use": return false
    case "var": return e.name === name
    case "app": return exprMentions(e.f, name) || exprMentions(e.x, name)
    case "ann": return exprMentions(e.expr, name) || exprMentions(e.type, name)
    case "proj": return exprMentions(e.target, name)
    case "binder": {
      // Shadowed if any param has this name.
      if (e.params.some(p => p.name === name)) {
        return e.params.some(p => p.type && exprMentions(p.type, name))
      }
      return e.params.some(p => p.type && exprMentions(p.type, name)) || exprMentions(e.body, name)
    }
    case "recType":
      return e.fields.some(f => f.type !== null && exprMentions(f.type, name))
    case "recValue": {
      if (e.fields.some(f => exprMentions(f.value, name) || (f.type && exprMentions(f.type, name))))
        return true
      if (e.members) {
        for (const m of e.members) {
          if (m.tag === "field") {
            if (exprMentions(m.value, name)) return true
            if (m.type && exprMentions(m.type, name)) return true
          } else if (m.tag === "let") {
            if (exprMentions(m.body, name)) return true
            if (m.type && exprMentions(m.type, name)) return true
            if (m.name === name) return false // subsequent members rebind
          } else if (m.tag === "test") {
            if (exprMentions(m.lhs, name) || exprMentions(m.rhs, name)) return true
          } else if (m.tag === "open") {
            if (exprMentions(m.expr, name)) return true
          }
        }
      }
      return false
    }
    case "match":
      return exprMentions(e.cond, name) || exprMentions(e.thenBody, name) || exprMentions(e.elseBody, name)
  }
}

// Peel a left-leaning app chain into head + args.
function peelApp(e: Expr): { head: Expr; args: Expr[] } {
  const args: Expr[] = []
  let cur = e
  while (cur.tag === "app") { args.unshift(cur.x); cur = cur.f }
  return { head: cur, args }
}

// Rewrite `select_lazy ({_} -> A) ({_} -> B) cond` → `match cond { TT => A; FF => B }`
// when both thunks have form `{_} -> body` (or `{x} -> body` with x unused).
// This avoids the cirToTree eager-K-body evaluation that fires on self-
// referential select_lazy uses (CLAUDE.md "Compiler workarounds"). The
// match-style desugaring (see "match" case below) wraps each arm in a
// closure over its free vars, side-stepping the K(body) construction whose
// body would otherwise be reduced eagerly at compile time.
//
// Trailing args (beyond the third) are passed through: the prelude
// select_lazy supports select-then-apply, and so does match's desugar.
function tryRewriteSelectLazy(
  e: Expr,
  lookupEntry: (name: string) => ScopeEntry | undefined,
): Expr | null {
  if (e.tag !== "app") return null
  const { head, args } = peelApp(e)
  if (head.tag !== "var" || head.name !== "select_lazy") return null
  if (args.length < 3) return null
  // Require `select_lazy` to resolve to something in scope (sanity: don't
  // rewrite if the name is unbound — let the normal path raise an error).
  if (!lookupEntry("select_lazy")?.tree) return null
  // `select` must also be in scope for the match desugaring.
  if (!lookupEntry("select")?.tree) return null
  const thenBody = asUnusedParamThunk(args[0])
  const elseBody = asUnusedParamThunk(args[1])
  if (thenBody === null || elseBody === null) return null
  const cond = args[2]
  let rewritten: Expr = { tag: "match", cond, thenBody, elseBody }
  // Re-apply any trailing args (select-then-apply).
  for (let i = 3; i < args.length; i++) {
    rewritten = { tag: "app", f: rewritten, x: args[i] }
  }
  return rewritten
}

// Optional callbacks threaded through Expr compilation. `recordTest` lets
// inline `{ ... test lhs = rhs ... }` blocks emit Test decls into the
// driver's `decls` array (Q2). `recordItem` mirrors parseProgram's
// per-item stats reporting so inline-block items show up in --stats-detail.
export interface CompileSinks {
  recordTest?: (lhs: Tree, rhs: Tree) => void
  recordOpen?: () => void
}

// Expr → Cir, with scope lookup and use-resolution.
function exprToCir(
  e: Expr,
  lookupEntry: (name: string) => ScopeEntry | undefined,
  resolveUse: (path: string) => ScopeEntry,
  sinks?: CompileSinks,
): Cir {
  const lookup = (name: string) => lookupEntry(name)?.tree
  switch (e.tag) {
    case "leaf": return { tag: "lit", t: LEAF }
    case "num": {
      const zero = lookup("zero")
      const succ = lookup("succ")
      if (!zero || !succ) throw new Error(`numeric literal ${e.value}: zero and succ must be in scope`)
      let result = zero
      for (let i = 0; i < e.value; i++) {
        result = applyTree(succ, result, APPLY_BUDGET)
      }
      return { tag: "lit", t: result }
    }
    case "str": return { tag: "lit", t: stringToTree(e.value) }
    case "var": {
      const entry = lookupEntry(e.name)
      return entry?.tree ? { tag: "lit", t: entry.tree } : { tag: "var", name: e.name }
    }
    case "hole": throw new Error("hole '_' cannot appear in untyped compilation")
    case "app": {
      // S2: rewrite `select_lazy (\_ -> A) (\_ -> B) cond [args...]` to
      // `match cond { TT => A; FF => B } [args...]` so the recursive arm
      // bodies don't hit cirToTree's eager K-body reduction. See
      // tryRewriteSelectLazy for details.
      const rewritten = tryRewriteSelectLazy(e, lookupEntry)
      if (rewritten !== null) return exprToCir(rewritten, lookupEntry, resolveUse, sinks)
      return { tag: "app",
        f: exprToCir(e.f, lookupEntry, resolveUse, sinks),
        x: exprToCir(e.x, lookupEntry, resolveUse, sinks),
      }
    }
    case "ann": return exprToCir(e.expr, lookupEntry, resolveUse, sinks) // erase type
    case "binder": {
      // Shadow binder params so they don't resolve to scope entries.
      // If a param has a recType annotation, carry its field names as metadata
      // so that projections (e.g. ks.field) work on bound variables.
      const paramNames = new Set(e.params.map((p, i) => p.name ?? `_anon${i}`))
      const paramEntries = new Map<string, ScopeEntry>()
      for (let i = 0; i < e.params.length; i++) {
        const name = e.params[i].name ?? `_anon${i}`
        if (e.params[i].type?.tag === "recType") {
          paramEntries.set(name, { fields: (e.params[i].type as any).fields.map((f: any) => f.name) })
        }
      }
      const shadowedLookup = (name: string): ScopeEntry | undefined => {
        if (paramNames.has(name)) return paramEntries.get(name)
        return lookupEntry(name)
      }

      let body = exprToCir(e.body, shadowedLookup, resolveUse, sinks)
      for (let i = e.params.length - 1; i >= 0; i--) {
        const name = e.params[i].name ?? `_anon${i}`
        body = { tag: "lam", x: name, body }
      }
      return body
    }
    case "recType": throw new Error("recType cannot appear in untyped compilation")
    case "recValue": {
      // If this recValue has members (let/test/open alongside fields),
      // process them to build a scoped lookup before compiling fields.
      // test/open members are wired through `sinks`: tests forward
      // (lhs, rhs) to the driver's decls collector; opens splice fields
      // into the local lookup (and don't escape this scope).
      let fieldLookup = lookupEntry
      if (e.members && e.members.length > 0) {
        const localScope = new Map<string, ScopeEntry>()
        const rebind = (name: string, entry: ScopeEntry) => {
          localScope.set(name, entry)
          const prevLookup = fieldLookup
          fieldLookup = (n: string) => localScope.get(n) ?? prevLookup(n)
        }
        for (const m of e.members) {
          if (m.tag === "let") {
            const tree = compileExpr(m.body, fieldLookup, resolveUse, sinks)
            let fields: string[] | undefined, fieldTrees: Tree[] | undefined
            if (m.type?.tag === "recType") {
              fields = (m.type as any).fields.map((f: any) => f.name)
            } else {
              const record = resolveExprRecord(m.body, fieldLookup, resolveUse)
              fields = record?.fields; fieldTrees = record?.fieldTrees
            }
            rebind(m.name, { tree, fields, fieldTrees })
          } else if (m.tag === "test") {
            // Q2: inline-block tests now flow to the driver via sinks.recordTest.
            // Without a sink we silently skip — same as the legacy behavior,
            // but only when there is no enclosing collector (e.g., during
            // inline elaboration of a typed binding, where tests would be
            // re-evaluated on every typecheck).
            if (sinks?.recordTest) {
              const lhs = compileExpr(m.lhs, fieldLookup, resolveUse, sinks)
              const rhs = compileExpr(m.rhs, fieldLookup, resolveUse, sinks)
              sinks.recordTest(lhs, rhs)
            }
          } else if (m.tag === "open") {
            // Inline `open expr`: resolve fields and splice them into the
            // local lookup chain. Doesn't escape this recValue's scope.
            const record = resolveExprRecord(m.expr, fieldLookup, resolveUse)
            if (!record || record.fields.length === 0)
              throw new Error("open (inline): expression has no known record fields")
            const targetTree = record.fieldTrees
              ? undefined
              : compileExpr(m.expr, fieldLookup, resolveUse, sinks)
            const n = record.fields.length
            for (let i = 0; i < n; i++) {
              const fieldTree = record.fieldTrees
                ? record.fieldTrees[i]
                : applyTree(targetTree!, accTree(record.fields[i]), APPLY_BUDGET)
              const fieldType = record.fieldTypes?.[i] ?? null
              const innerFields = (record as any).fieldInnerFields?.[i] as string[] | undefined
              const innerTrees = (record as any).fieldInnerTrees?.[i] as Tree[] | undefined
              rebind(record.fields[i], { tree: fieldTree, type: fieldType, fields: innerFields, fieldTrees: innerTrees })
            }
            sinks?.recordOpen?.()
          }
        }
      }
      // If this recValue carries a `trailing` expression (block-with-
      // trailing-expr that contained test/open members), evaluate the
      // trailing body in the local scope and return its value instead
      // of the Church-encoded record. fields[] is expected to be empty
      // in this case (parser invariant).
      if (e.trailing) {
        if (e.fields.length !== 0)
          throw new Error("recValue: 'trailing' is only valid when fields are empty")
        return exprToCir(e.trailing, fieldLookup, resolveUse, sinks)
      }
      // §2.6 record: {x := a; y := b} → record_val ["x","y"] [list_const a, list_const b]
      // — a `prod` over a string-interned name header, read by name through the
      // cut. (record_val/list_const must be in scope, like `match` needs `prod`.)
      const recordValEntry = lookupEntry("record_val")
      const listConstEntry = lookupEntry("list_const")
      if (!recordValEntry?.tree || !listConstEntry?.tree)
        throw new Error("record literal '{ := }': 'record_val' and 'list_const' must be in scope (open the kernel prelude)")
      const recordVal: Cir = { tag: "lit", t: recordValEntry.tree }
      const listConst: Cir = { tag: "lit", t: listConstEntry.tree }
      // names header: a closed cons-chain of string tags.
      let namesTree: Tree = LEAF
      for (let i = e.fields.length - 1; i >= 0; i--)
        namesTree = fork(stringToTree(e.fields[i].name), namesTree)
      // payload: a cons-chain of const-wrapped field values (cons = `t h tl`).
      const consCir = (h: Cir, tl: Cir): Cir => cap(cap({ tag: "lit", t: LEAF }, h), tl)
      let payloadCir: Cir = { tag: "lit", t: LEAF }
      for (let i = e.fields.length - 1; i >= 0; i--) {
        const fc = exprToCir(e.fields[i].value, fieldLookup, resolveUse, sinks)
        payloadCir = consCir(cap(listConst, fc), payloadCir)
      }
      return cap(cap(recordVal, { tag: "lit", t: namesTree }), payloadCir)
    }
    case "use": {
      const entry = resolveUse(e.path)
      return { tag: "lit", t: entry.tree! }
    }
    case "proj": {
      // r.x is the §2.6 cut `r (acc x)`. When the target is a statically-known
      // record we keep the compile-time collapse (return the field's tree, or
      // validate the name); otherwise we emit the runtime cut so projection
      // works on any product value (e.g. a metadata record passed at runtime).
      const record = resolveExprRecord(e.target, lookupEntry, resolveUse)
      if (record) {
        const idx = record.fields.indexOf(e.field)
        if (idx < 0)
          throw new Error(`projection '.${e.field}': field not found (available: ${record.fields.join(", ")})`)
        if (record.fieldTrees) return { tag: "lit", t: record.fieldTrees[idx] }
      }
      const target = exprToCir(e.target, lookupEntry, resolveUse, sinks)
      return cap(target, { tag: "lit", t: accTree(e.field) })
    }
    case "match": {
      // Desugar to closed-branch select-then-apply.
      // `match cond { TT => e1; FF => e2 }` becomes
      // `select ({fvs...} -> e1) ({fvs...} -> e2) cond fv1 fv2 ... fvn`
      // where fvs is the union of free vars across both arms (only names
      // that aren't already closed top-level lits).
      const selectEntry = lookupEntry("select")
      if (!selectEntry?.tree)
        throw new Error("match: 'select' must be in scope (import prelude)")

      const condCir = exprToCir(e.cond, lookupEntry, resolveUse, sinks)
      const thenCir = exprToCir(e.thenBody, lookupEntry, resolveUse, sinks)
      const elseCir = exprToCir(e.elseBody, lookupEntry, resolveUse, sinks)

      const fvs: string[] = []
      const seen = new Set<string>()
      collectFreeVars(thenCir, new Set(), fvs, seen)
      collectFreeVars(elseCir, new Set(), fvs, seen)

      const wrap = (body: Cir): Cir => {
        let b = body
        for (let i = fvs.length - 1; i >= 0; i--) b = { tag: "lam", x: fvs[i], body: b }
        return b
      }
      const branchTT = wrap(thenCir)
      const branchFF = wrap(elseCir)

      let out: Cir = cap(cap(cap({ tag: "lit", t: selectEntry.tree }, branchTT), branchFF), condCir)
      for (const v of fvs) out = cap(out, { tag: "var", name: v })
      return out
    }
  }
}

// Resolve record metadata known at compile time (for projection/open).
function resolveExprRecord(
  e: Expr,
  lookupEntry: (name: string) => ScopeEntry | undefined,
  resolveUse: (path: string) => ScopeEntry,
): { fields: string[]; fieldTrees?: Tree[]; fieldTypes?: (Tree | null)[]; fieldInnerFields?: (string[] | undefined)[]; fieldInnerTrees?: (Tree[] | undefined)[] } | undefined {
  if (e.tag === "var") {
    const entry = lookupEntry(e.name)
    return entry?.fields ? { fields: entry.fields, fieldTrees: entry.fieldTrees, fieldTypes: entry.fieldTypes } : undefined
  }
  if (e.tag === "use") {
    const entry = resolveUse(e.path) as any
    return entry.fields ? { fields: entry.fields, fieldTrees: entry.fieldTrees, fieldTypes: entry.fieldTypes, fieldInnerFields: entry.fieldInnerFields, fieldInnerTrees: entry.fieldInnerTrees } : undefined
  }
  // Application where the argument is a binder returning a recValue:
  // e.g. fix({ks : {...}} -> { f1 := ...; f2 := ... }) → fields from the recValue.
  // This propagates field metadata through higher-order patterns like fix.
  if (e.tag === "app" && e.x.tag === "binder" && e.x.body.tag === "recValue") {
    return { fields: e.x.body.fields.map(f => f.name) }
  }
  if (e.tag === "recValue") {
    // If this recValue has let/open members, they shadow scope for the
    // field values. Process them to build the effective lookup before
    // compiling fields. Tests are skipped here — resolveExprRecord
    // shouldn't have side effects (it may be called speculatively).
    let fieldLookup = lookupEntry
    if (e.members && e.members.length > 0) {
      const localScope = new Map<string, ScopeEntry>()
      for (const m of e.members) {
        if (m.tag === "let") {
          const tree = compileExpr(m.body, fieldLookup, resolveUse)
          const inner = resolveExprRecord(m.body, fieldLookup, resolveUse)
          localScope.set(m.name, { tree, fields: inner?.fields, fieldTrees: inner?.fieldTrees })
          const prevLookup = fieldLookup
          fieldLookup = (name: string) => localScope.get(name) ?? prevLookup(name)
        } else if (m.tag === "open") {
          const rec = resolveExprRecord(m.expr, fieldLookup, resolveUse)
          if (!rec) continue
          const targetTree = rec.fieldTrees
            ? undefined
            : compileExpr(m.expr, fieldLookup, resolveUse)
          const n = rec.fields.length
          for (let i = 0; i < n; i++) {
            const ft = rec.fieldTrees
              ? rec.fieldTrees[i]
              : applyTree(targetTree!, accTree(rec.fields[i]), APPLY_BUDGET)
            localScope.set(rec.fields[i], { tree: ft, type: rec.fieldTypes?.[i] ?? null })
          }
          const prevLookup = fieldLookup
          fieldLookup = (name: string) => localScope.get(name) ?? prevLookup(name)
        }
      }
    }
    return {
      fields: e.fields.map(f => f.name),
      fieldTrees: e.fields.map(f => compileExpr(f.value, fieldLookup, resolveUse)),
    }
  }
  if (e.tag === "proj") {
    // Nested projection: target.field — would need the inner record's field type
    // which itself is a record. Not supported yet.
    return undefined
  }
  return undefined
}

function compileExpr(
  e: Expr,
  lookupEntry: (name: string) => ScopeEntry | undefined,
  resolveUse: (path: string) => ScopeEntry,
  sinks?: CompileSinks,
): Tree {
  return cirToTree(eliminateLams(exprToCir(e, lookupEntry, resolveUse, sinks)))
}

// ──────────── 5b. Tree-level abstraction + kernel query helpers ─────────

// abstractTree(h, body): given a tree `body` containing subtree `h`,
// produce a tree F such that apply(F, v) = body[h := v].
// Same algorithm as eliminateLams but on Tree with hash-cons identity for variable check.

function containsTree(h: Tree, t: Tree): boolean {
  if (treeEqual(h, t)) return true
  if (t.tag === "leaf") return false
  if (t.tag === "stem") return containsTree(h, t.child)
  return containsTree(h, t.left) || containsTree(h, t.right)
}

function abstractTree(h: Tree, body: Tree): Tree {
  if (treeEqual(body, h)) return I_TREE
  if (!containsTree(h, body)) return fork(LEAF, body) // K body
  if (body.tag === "fork") {
    const l = abstractTree(h, body.left)
    const r = abstractTree(h, body.right)
    // A normal-form fork is data, not necessarily an application that should
    // reduce as left(right). Rebuild fork(l, r) as (leaf l) r.
    const stemLeft = fork(stem(fork(LEAF, LEAF)), l) // S (K LEAF) l
    return fork(stem(stemLeft), r) // S stemLeft r
  }
  if (body.tag === "stem") {
    // Rebuild stem(x) as leaf x.
    const inner = abstractTree(h, body.child)
    return fork(stem(fork(LEAF, LEAF)), inner) // S (K LEAF) inner
  }
  return fork(LEAF, body) // K body (leaf case, h not present)
}

// Kernel tree helpers — tree-level pair operations.
// pair = fork(l, r). pair_fst extracts left, pair_snd extracts right.
// These mirror the Disp prelude's pair_fst/pair_snd but operate on Tree directly.

function treePairFst(p: Tree): Tree | null {
  return p.tag === "fork" ? p.left : null
}
function treePairSnd(p: Tree): Tree | null {
  return p.tag === "fork" ? p.right : null
}

// type_meta(T) = pair_snd(pair_snd(T)) — extracts metadata from a wait-based type.
function typeMetaTree(t: Tree): Tree | null {
  const inner = treePairSnd(t)
  return inner ? treePairSnd(inner) : null
}

// checker_sig(checker) = pair_fst(wait checker t) — signature of a type checker.
// wait(checker)(t) reduces to a tree; pair_fst gives the constant signature.
function checkerSig(checker: Tree): Tree {
  const waitResult = applyTree(applyTree(checker, LEAF, APPLY_BUDGET), LEAF, APPLY_BUDGET)
  const sig = treePairFst(waitResult)
  if (!sig) throw new Error("checkerSig: expected fork from wait(checker)(t)")
  return sig
}

// Identifier interning — canonical Scott byte-list trees for source
// identifiers. Used by record-projection desugaring (r.x → proj r "x")
// so the name "x" becomes a tree literal that can be compared via
// tree_eq (= hash-cons identity) at compile time and runtime.
//
// Encoding: each codepoint is a succ-chain Nat; the list is built with
// `string_cons` from right to left, terminated by `empty_string`. The
// Map is a parse-time cache; hash-cons would dedupe anyway, but caching
// avoids rebuilding succ-chains for repeated identifiers.
const internedNames = new Map<string, Tree>()

function internName(name: string, lookupEntry: (n: string) => ScopeEntry | undefined): Tree {
  const cached = internedNames.get(name)
  if (cached) return cached

  const zeroTree = lookupEntry("zero")?.tree
  const succTree = lookupEntry("succ")?.tree
  const emptyStr = lookupEntry("empty_string")?.tree
  const consStr  = lookupEntry("string_cons")?.tree
  if (!zeroTree || !succTree || !emptyStr || !consStr)
    throw new Error("internName: zero/succ/empty_string/string_cons must be in scope")

  // Right-to-left fold: result = cons(byte_0, cons(byte_1, ... empty))
  let result = emptyStr
  for (let i = name.length - 1; i >= 0; i--) {
    const code = name.codePointAt(i)!
    let byteTree = zeroTree
    for (let j = 0; j < code; j++) byteTree = applyTree(succTree, byteTree, APPLY_BUDGET)
    result = applyTree(applyTree(consStr, byteTree, APPLY_BUDGET), result, APPLY_BUDGET)
  }
  internedNames.set(name, result)
  return result
}

// Kernel query helpers. These are discovered from ordinary scope after
// imports. A file that opens kernel/prelude.disp gets Pi, Type, Hyp, Nat,
// etc. without a separate keyword-level trust channel.

interface KernelHelpers {
  isUniverse(t: Tree): boolean
  isPi(t: Tree): boolean
  isNeutral(t: Tree): boolean
  piDomain(t: Tree): Tree
  piCodFn(t: Tree): Tree
  makeHyp(type: Tree, id: Tree): Tree
}

function makeKernelHelpers(lookupEntry: (name: string) => ScopeEntry | undefined): KernelHelpers | null {
  // New two-Σ-op kernel: a type is `wait (make_recognizer body) meta`. Type-
  // formers are told apart by their recognizer SIGNATURE (`pair_fst T`, which is
  // constant per former, independent of the type's parameters). The MetaShape
  // meta is a §2.6 headered record (read by name through the cut), so the host
  // recovers a type's `recognizer_params` by delegating to the in-language
  // `meta_params` accessor rather than reading a fixed positional slot.
  const Pi = lookupEntry("Pi")?.tree
  const Type = lookupEntry("Type")?.tree
  const make_hyp = lookupEntry("Hyp")?.tree ?? lookupEntry("make_hyp")?.tree
  const meta_params = lookupEntry("meta_params")?.tree
  // Pi's recognizer_params is the §2.6 record { dom, cod } (§12); the host reads
  // its fields by name through the in-language `field` cut and the rp_* tags.
  const field_fn = lookupEntry("field")?.tree
  const rp_dom = lookupEntry("rp_dom")?.tree
  const rp_cod = lookupEntry("rp_cod")?.tree

  if (!Pi && !Type && !make_hyp) return null

  const samplePi = Pi ? applyTree(applyTree(Pi, LEAF, APPLY_BUDGET), I_TREE, APPLY_BUDGET) : null
  const piSig = samplePi ? treePairFst(samplePi) : null
  const typeSig = Type ? treePairFst(Type) : null
  const sampleHyp = make_hyp ? applyTree(applyTree(make_hyp, LEAF, APPLY_BUDGET), LEAF, APPLY_BUDGET) : null
  const hypSig = sampleHyp ? treePairFst(sampleHyp) : null

  function sigMatches(sig: Tree | null, t: Tree): boolean {
    if (!sig) return false
    const ts = treePairFst(t)
    return ts !== null && treeEqual(ts, sig)
  }
  // recognizer_params = meta_params (type_meta T), via the in-language accessor
  // (the meta is a §2.6 record read by name). For Pi, params is the record
  // { dom, cod }, whose fields are read by name (not positionally).
  function piParams(t: Tree): Tree {
    const meta = typeMetaTree(t)
    if (!meta) throw new Error("piParams: not a valid type tree")
    if (!meta_params) throw new Error("piParams: meta_params not in scope")
    const params = applyTree(meta_params, meta, APPLY_BUDGET)
    if (!params) throw new Error("piParams: params slot missing")
    return params
  }
  // Read a named field of Pi's params record via the in-language `field` cut.
  function piField(t: Tree, key: Tree | undefined, slot: string): Tree {
    if (!field_fn || !key) throw new Error(`piField: field/${slot} tag not in scope`)
    const r = applyTree(applyTree(field_fn, piParams(t), APPLY_BUDGET), key, APPLY_BUDGET)
    if (!r) throw new Error(`piField: ${slot} slot missing`)
    return r
  }

  return {
    isUniverse(t) { return sigMatches(typeSig, t) },
    isPi(t) { return sigMatches(piSig, t) },
    isNeutral(t) { return sigMatches(hypSig, t) },
    piDomain(t) { return piField(t, rp_dom, "dom") },
    piCodFn(t) { return piField(t, rp_cod, "cod") },
    makeHyp(type, id) {
      if (!make_hyp) throw new Error("makeHyp: make_hyp not in scope")
      return applyTree(applyTree(make_hyp, type, APPLY_BUDGET), id, APPLY_BUDGET)
    },
  }
}

// checkAsType(e, ctx): compile an expression as a type, returning both
// the compiled tree and the universe it lives in. For binders, this
// triggers Pi construction. For other expressions, it infers and verifies.
function checkAsType(e: Expr, ctx: ElabCtx): { tree: Tree; universe: Tree | null } {
  const k = ctx.kernel
  if (!k) {
    const tree = compileExpr(e, ctx.lookupEntry, ctx.resolveUse)
    return { tree, universe: null }
  }

  // Binder → Pi type construction.
  if (e.tag === "binder") {
    const Type = ctx.lookupEntry("Type")?.tree
    const Pi = ctx.lookupEntry("Pi")?.tree
    if (!Type || !Pi) {
      const tree = compileExpr(e, ctx.lookupEntry, ctx.resolveUse)
      return { tree, universe: null }
    }

    // Process one param, recursing for the rest.
    const param = e.params[0]
    if (!param.type) throw new Error("Pi domain requires a type annotation")
    const paramName = param.name ?? `_anon0`

    const { tree: A_tree } = checkAsType(param.type, ctx)

    // Recurse for the inner type (remaining params + body) with the
    // parameter available as a kernel hypothesis. The resulting codomain
    // tree may mention that hypothesis; abstractTree turns it into the
    // Pi codomain function.
    const innerExpr: Expr = e.params.length > 1
      ? { tag: "binder", params: e.params.slice(1), body: e.body }
      : e.body

    const id = fork(A_tree, fork(buildNat(ctx.scopeDepth), buildNat(0)))
    const hyp = k.makeHyp(A_tree, id)
    const prevLookup = ctx.lookupEntry
    const hypEntry: ScopeEntry = { tree: hyp, type: A_tree }
    ctx.lookupEntry = (name: string) => name === paramName ? hypEntry : prevLookup(name)
    ctx.scopeDepth++
    let B_tree: Tree
    try {
      const checked = checkAsType(innerExpr, ctx)
      B_tree = checked.tree
    } finally {
      ctx.lookupEntry = prevLookup
      ctx.scopeDepth--
    }

    const codFn = abstractTree(hyp, B_tree)

    const piTree = applyTree(applyTree(Pi, A_tree, APPLY_BUDGET), codFn, APPLY_BUDGET)
    // Universe stratification dropped — every Pi lives in the single
    // Type, not Type k. The sample-style call here is equivalent.
    const piUniv = Type
    return { tree: piTree, universe: piUniv }
  }

  // Non-binder: infer type and check it's a universe
  const { tree, type } = infer(e, ctx)
  if (type !== null && k.isUniverse(type)) {
    return { tree, universe: type }
  }
  // Type is unknown or not a universe — return what we have
  return { tree, universe: type }
}

// ─────────────────────── 5c. Elaborator (check/infer) ───────────────────

// Scott-encoded Bool: TT = K K = fork(LEAF, K), FF = K (K I) per spec §4.5.
// Imported from tree.ts so the host's tree_eq fast-path and the elaborator's
// predicate validation agree on the canonical shapes.
const TT = SCOTT_TT
// New-core recognizers return an Ok-wrapped verdict (`Ok TT` = fork(LEAF, TT));
// a bare TT also counts (e.g. Scott-Bool predicates). The elaborator's
// type-check accepts either.
const OK_TT = fork(LEAF, TT)
function verdictOk(ok: Tree): boolean { return treeEqual(ok, TT) || treeEqual(ok, OK_TT) }

type ElabCtx = {
  lookupEntry: (name: string) => ScopeEntry | undefined
  resolveUse: (path: string) => ScopeEntry
  kernel: KernelHelpers | null
  scopeDepth: number
  debugTypeCheck: boolean
}

// Defense-in-depth: verify the kernel agrees with the elaborator's type dispatch.
// Gated behind debugTypeCheck — never on in production.
function assertTypeCheck(tree: Tree, expected: Tree | null, ctx: ElabCtx): Tree {
  if (ctx.debugTypeCheck && expected !== null) {
    const ok = applyTree(expected, tree, APPLY_BUDGET)
    if (!verdictOk(ok)) {
      throw new Error(
        `defense-in-depth: type check failed\n` +
        `  tree:     ${prettyTree(tree)}\n` +
        `  result:   ${prettyTree(ok)}`
      )
    }
  }
  return tree
}

// check(e, expected, ctx): compile e, expecting it to have type `expected`.
// When expected is null, delegates to compileExpr (untyped path).
function check(e: Expr, expected: Tree | null, ctx: ElabCtx): Tree {
  if (expected === null) return compileExpr(e, ctx.lookupEntry, ctx.resolveUse)

  const k = ctx.kernel
  if (!k) {
    // No kernel helpers — fall through to untyped + predicate check
    const tree = compileExpr(e, ctx.lookupEntry, ctx.resolveUse)
    const ok = applyTree(expected, tree, APPLY_BUDGET)
    if (!verdictOk(ok))
      throw new Error(`type check failed (no kernel helpers)`)
    return tree
  }

  // Type-directed dispatch for binders
  if (e.tag === "binder") {
    // Case 2: expected is a universe → binder means Pi
    if (k.isUniverse(expected)) {
      // Delegate to checkAsType which handles Pi construction with level inference
      const { tree } = checkAsType(e, ctx)
      return assertTypeCheck(tree, expected, ctx)
    }

    // Case 3: expected is a Pi → binder means lambda
    if (k.isPi(expected)) {
      const D = k.piDomain(expected)
      const codFn = k.piCodFn(expected)

      if (e.params.length === 0) throw new Error("empty binder against Pi")
      const param = e.params[0]
      const paramName = param.name ?? `_anon0`

      // Check domain annotation if present
      if (param.type) {
        const { tree: A_tree } = infer(param.type, ctx)
        if (!treeEqual(A_tree, D))
          throw new Error(`domain annotation doesn't match expected Pi domain`)
      }

      // Compile the binder as a lambda using the Cir pipeline (which handles
      // bracket abstraction correctly via named variables). Type-check the body
      // by binding the parameter to a hypothesis with the domain type, so that
      // inner check/infer calls see the correct type information.
      const piMeta = typeMetaTree(expected)
      const hyp = k.makeHyp(D, piMeta ?? fork(D, codFn))

      // Shadow param with hypothesis for type tracking
      const prevLookup = ctx.lookupEntry
      const hypEntry: ScopeEntry = { tree: hyp, type: D }

      // For inner type checking, bind param to hypothesis
      // But for compilation, we use the Cir pipeline which does bracket abstraction by name
      // So we DON'T bind the param tree — we let exprToCir see it as a free variable
      // and bracket-abstract it normally. We only set the type for inner checks.
      const paramTypeEntry: ScopeEntry = { type: D }
      ctx.lookupEntry = (name: string) => name === paramName ? paramTypeEntry : prevLookup(name)
      ctx.scopeDepth++

      // Verify the body type-checks: create hypothesis scope for type checking
      const innerExpr: Expr = e.params.length > 1
        ? { tag: "binder", params: e.params.slice(1), body: e.body }
        : e.body

      // For the body type check, bind the param to the actual hypothesis tree
      const checkLookup = (name: string): ScopeEntry | undefined =>
        name === paramName ? hypEntry : prevLookup(name)
      const savedLookup = ctx.lookupEntry
      ctx.lookupEntry = checkLookup
      const result_type = applyTree(codFn, hyp, APPLY_BUDGET)

      // Type-check the body (this validates types but we don't use the tree)
      // Only do this if there's meaningful type info to propagate
      if (k.isPi(result_type) || k.isUniverse(result_type) || k.isNeutral(result_type)) {
        // Deep type check — validates body against codomain
        check(innerExpr, result_type, ctx)
      }

      // Restore and compile using the Cir pipeline for correct bracket abstraction
      ctx.lookupEntry = prevLookup
      ctx.scopeDepth--

      return assertTypeCheck(compileExpr(e, ctx.lookupEntry, ctx.resolveUse), expected, ctx)
    }
  }

  // Case 4 / fallback: infer then verify
  const { tree, type } = infer(e, ctx)
  if (type !== null && treeEqual(type, expected)) return tree
  // Fall back to predicate check
  const ok = applyTree(expected, tree, APPLY_BUDGET)
  if (!verdictOk(ok))
    throw new Error(`type check failed`)
  return tree
}

// infer(e, ctx): compile e and infer its type.
function infer(e: Expr, ctx: ElabCtx): { tree: Tree; type: Tree | null } {
  const k = ctx.kernel

  switch (e.tag) {
    case "var": {
      const entry = ctx.lookupEntry(e.name)
      if (!entry?.tree) {
        // Free variable — compile via untyped path
        const tree = compileExpr(e, ctx.lookupEntry, ctx.resolveUse)
        return { tree, type: null }
      }
      return { tree: entry.tree, type: entry.type ?? null }
    }

    case "app": {
      const { tree: f_tree, type: f_type } = infer(e.f, ctx)

      if (k && f_type !== null && k.isPi(f_type)) {
        const D = k.piDomain(f_type)
        const codFn = k.piCodFn(f_type)
        const x_tree = check(e.x, D, ctx)
        const result_tree = applyTree(f_tree, x_tree, APPLY_BUDGET)
        const result_type = applyTree(codFn, x_tree, APPLY_BUDGET)
        return { tree: result_tree, type: result_type }
      }

      // Untyped fallback
      const x_tree = check(e.x, null, ctx)
      const result_tree = applyTree(f_tree, x_tree, APPLY_BUDGET)
      return { tree: result_tree, type: null }
    }

    case "ann": {
      // Compile annotation as a type (handles binders → Pi construction).
      const { tree: T_tree, universe: T_univ } = checkAsType(e.type, ctx)
      if (k && T_univ !== null && !k.isUniverse(T_univ))
        throw new Error("annotation is not a type")
      const e_tree = check(e.expr, T_tree, ctx)
      return { tree: e_tree, type: T_tree }
    }

    case "binder": {
      // Compile the binder as a lambda using the Cir pipeline
      const lam = compileExpr(e, ctx.lookupEntry, ctx.resolveUse)

      if (!k) return { tree: lam, type: null }

      // If domain annotated, infer a Pi type for the result
      const param = e.params[0]
      if (param?.type) {
        const { tree: A_tree } = checkAsType(param.type, ctx)
        const paramName = param.name ?? `_anon0`
        const Pi = ctx.lookupEntry("Pi")?.tree
        if (Pi) {
          const id = fork(A_tree, fork(buildNat(ctx.scopeDepth), buildNat(0)))
          const hyp = k.makeHyp(A_tree, id)
          const prevLookup = ctx.lookupEntry
          const hypEntry: ScopeEntry = { tree: hyp, type: A_tree }
          ctx.lookupEntry = (name: string) => name === paramName ? hypEntry : prevLookup(name)
          ctx.scopeDepth++
          const innerExpr2: Expr = e.params.length > 1
            ? { tag: "binder", params: e.params.slice(1), body: e.body }
            : e.body
          const { type: body_type } = infer(innerExpr2, ctx)
          ctx.lookupEntry = prevLookup
          ctx.scopeDepth--

          if (body_type !== null) {
            const codFn = abstractTree(hyp, body_type)
            const pi = applyTree(applyTree(Pi, A_tree, APPLY_BUDGET), codFn, APPLY_BUDGET)
            return { tree: lam, type: pi }
          }
        }
      }

      return { tree: lam, type: null }
    }

    case "leaf":
      return { tree: LEAF, type: null }

    case "num": {
      const tree = compileExpr(e, ctx.lookupEntry, ctx.resolveUse)
      const Nat = ctx.lookupEntry("Nat")?.tree
      return { tree, type: Nat ?? null }
    }

    case "proj": {
      const tree = compileExpr(e, ctx.lookupEntry, ctx.resolveUse)
      return { tree, type: null }
    }

    case "recValue": {
      const tree = compileExpr(e, ctx.lookupEntry, ctx.resolveUse)
      return { tree, type: null }
    }

    default: {
      const tree = compileExpr(e, ctx.lookupEntry, ctx.resolveUse)
      return { tree, type: null }
    }
  }
}

// Helper to build a Nat tree from a number (for hypothesis identity).
function buildNat(n: number): Tree {
  // zero = LEAF, succ(n) = fork(fork(LEAF, LEAF), n) = stem(LEAF) applied to n
  let result: Tree = LEAF
  for (let i = 0; i < n; i++) {
    result = fork(fork(LEAF, LEAF), result)
  }
  return result
}

// natLitTree(n): the lib's canonical Nat for `n` — zero = LEAF, succ(m) =
// `t t m` = fork(LEAF, m). Matches `succ`/`zero` applied in scope (unlike
// buildNat, which is only an opaque hypothesis-id encoding).
function natLitTree(n: number): Tree {
  let result: Tree = LEAF
  for (let i = 0; i < n; i++) result = fork(LEAF, result)
  return result
}

// stringToTree(s): a string literal as a List of codepoint Nats (cons = fork,
// nil = LEAF) — bit-identical to the array literal `[c0, c1, …]` of its
// codepoints, so a string is a genuine String value and a deterministic,
// distinct tag per spelling. Reused to intern record field-name identifiers.
export function stringToTree(s: string): Tree {
  const codes = [...s].map(c => c.codePointAt(0)!)
  let result: Tree = LEAF
  for (let i = codes.length - 1; i >= 0; i--) result = fork(natLitTree(codes[i]), result)
  return result
}

// accTree(name): the §2.6 accessor for a field name — `acc name = inj name unit
// = fork(name, LEAF)`, with the name interned as a string tag. Applying a
// product (record) to it performs the cut and yields the named field.
function accTree(name: string): Tree {
  return fork(stringToTree(name), LEAF)
}

// ──────────────────────────── 6. Driver ─────────────────────────────────

export type Decl =
  | { kind: "Def"; name: string; tree: Tree; type?: Tree | null; fields?: string[]; fieldTrees?: Tree[] }
  | { kind: "Test"; lhs: Tree; rhs: Tree }

export type ParseItemStats = {
  kind: "let" | "test" | "open" | "field"
  name?: string
  testIndex?: number
  sourcePath?: string
  depth: number
  stats: ApplyStats
}

export type ParseProgramOptions = {
  onItem?: (item: ParseItemStats) => void
  debugTypeCheck?: boolean
}

export function parseProgram(src: string, sourcePath?: string, options: ParseProgramOptions = {}): Decl[] {
  const stack: Map<string, ScopeEntry>[] = [new Map()]
  const decls: Decl[] = []
  const dirStack = [sourcePath ? dirname(pathResolve(sourcePath)) : process.cwd()]
  const sourceStack = [sourcePath ? pathResolve(sourcePath) : undefined]
  const loadedFiles = new Set<string>() // cycle detection
  let compiledTestIndex = 0

  const lookupEntry = (name: string): ScopeEntry | undefined => {
    for (let i = stack.length - 1; i >= 0; i--) {
      const e = stack[i].get(name)
      if (e !== undefined) return e
    }
    return undefined
  }
  const define = (name: string, entry: ScopeEntry) => stack[stack.length - 1].set(name, entry)

  function resolveUse(path: string): ScopeEntry {
    const abs = pathResolve(dirStack[dirStack.length - 1], path)
    if (loadedFiles.has(abs)) throw new Error(`use: circular dependency on ${abs}`)
    loadedFiles.add(abs)
    const fileSrc = readFileSync(abs, "utf-8")
    dirStack.push(dirname(abs))
    sourceStack.push(abs)
    // Push a new scope frame for the used file.
    stack.push(new Map())
    const fileDecls: Decl[] = []
    const items = parseItems(fileSrc)
    // Detect whether this file uses the new field syntax.
    // If any field members exist, only fields export. Otherwise fall back
    // to legacy mode where all lets export (for backward compat during migration).
    const hasFields = items.some(it => it.tag === "field")
    try {
      for (const it of items) {
        runItem(it, fileDecls, !hasFields)
      }
    } finally {
      const fileScope = stack.pop()!
      dirStack.pop()
      sourceStack.pop()
      loadedFiles.delete(abs)
    }
    // Collect the file's top-level defs as a record. Each field can
    // itself be a record (carrying nested fields metadata) — those
    // propagate so that downstream `kernel.hyp_reduce`-style projections
    // work across `open use` boundaries.
    const fieldNames: string[] = []
    const fieldTrees: Tree[] = []
    const fieldTypes: (Tree | null)[] = []
    const fieldInnerFields: (string[] | undefined)[] = []
    const fieldInnerTrees: (Tree[] | undefined)[] = []
    for (const d of fileDecls) {
      if (d.kind === "Def") {
        fieldNames.push(d.name)
        fieldTrees.push(d.tree)
        fieldTypes.push(d.type ?? null)
        fieldInnerFields.push(d.fields)
        fieldInnerTrees.push(d.fieldTrees)
      }
    }
    // Church-encode: \sel. sel v1 v2 ... vn
    const n = fieldTrees.length
    if (n === 0) return { tree: LEAF, fields: [] }
    // Build as Cir, then compile
    const selName = "__use_sel"
    let body: Cir = { tag: "var", name: selName }
    for (const ft of fieldTrees) body = cap(body, { tag: "lit", t: ft })
    const cir: Cir = { tag: "lam", x: selName, body }
    const tree = cirToTree(eliminateLams(cir))
    return { tree, fields: fieldNames, fieldTrees, fieldTypes, fieldInnerFields, fieldInnerTrees }
  }

  function recordItem(kind: "let" | "test" | "open" | "field", name?: string, testIndex?: number): void {
    options.onItem?.({
      kind,
      name,
      testIndex,
      sourcePath: sourceStack[sourceStack.length - 1],
      depth: sourceStack.length - 1,
      stats: getApplyStats(),
    })
  }

  function getElabCtx(): ElabCtx {
    return {
      lookupEntry,
      resolveUse,
      kernel: makeKernelHelpers(lookupEntry),
      scopeDepth: stack.length,
      debugTypeCheck: options.debugTypeCheck ?? false,
    }
  }

  function compileBinding(name: string, type: Expr | null | undefined, body: Expr, sinks?: CompileSinks): { tree: Tree; type?: Tree | null; fields?: string[]; fieldTrees?: Tree[] } {
    let tree: Tree, inferredType: Tree | null = null
    const ctx = getElabCtx()

    if (type != null && type.tag !== "recType" && ctx.kernel) {
      // Typed binding with kernel available: compile annotation as a type.
      // checkAsType handles binders (→ Pi construction) and infers universe levels.
      // FIXME(Q2): inline tests inside typed binding bodies (e.g.
      //   `let foo : T = { test ... ; body }`) are not yet wired — check/infer
      //   don't thread the CompileSinks. Practical impact is small because
      //   typed bindings rarely contain inline tests; lift them to the
      //   enclosing untyped scope if you need them. To remove this caveat,
      //   add `sinks?` to ElabCtx and propagate through check/infer.
      const { tree: type_tree, universe } = checkAsType(type, ctx)
      if (universe !== null && !ctx.kernel.isUniverse(universe))
        throw new Error(`annotation for '${name}' is not a type`)
      try {
        tree = check(body, type_tree, ctx)
      } catch (e: any) {
        throw new Error(`type check failed for '${name}': ${e.message}`)
      }
      inferredType = type_tree
    } else {
      // Untyped or no kernel — use existing compileExpr path
      tree = compileExpr(body, lookupEntry, resolveUse, sinks)
    }

    let fields: string[] | undefined, fieldTrees: Tree[] | undefined
    if (type?.tag === "recType") {
      fields = (type as any).fields.map((f: any) => f.name)
    } else {
      const record = resolveExprRecord(body, lookupEntry, resolveUse)
      fields = record?.fields; fieldTrees = record?.fieldTrees
    }
    define(name, { tree, type: inferredType, fields, fieldTrees })
    return { tree, type: inferredType, fields, fieldTrees }
  }

  // Build a CompileSinks for the given target/decl array. Tests emitted by
  // inline recValue blocks (`{ ... test x = y ... }`) flow through this
  // sink — they're pushed into the same `target` array as top-level tests
  // and reported via recordItem so --stats-detail sees them.
  function makeSinks(target: Decl[]): CompileSinks {
    return {
      recordTest(lhs, rhs) {
        compiledTestIndex++
        target.push({ kind: "Test", lhs, rhs })
        recordItem("test", undefined, compiledTestIndex)
      },
      recordOpen() { recordItem("open") },
    }
  }

  function runItem(it: RecMember, target: Decl[], isExport: boolean): void {
    const sinks = makeSinks(target)
    switch (it.tag) {
      case "field": {
        const result = compileBinding(it.name, it.type, it.value, sinks)
        target.push({ kind: "Def", name: it.name, tree: result.tree, type: result.type, fields: result.fields, fieldTrees: result.fieldTrees })
        // Register the canonical tree_eq tree id with the runtime fast-path on first definition.
        if (it.name === "tree_eq" && getTreeEqId() === -1) setTreeEqId(result.tree.id)
        recordItem("field", it.name)
        return
      }
      case "let": {
        const result = compileBinding(it.name, it.type, it.body, sinks)
        if (isExport) {
          // Legacy mode: top-level let exports (for files not yet migrated)
          target.push({ kind: "Def", name: it.name, tree: result.tree, type: result.type, fields: result.fields, fieldTrees: result.fieldTrees })
        }
        if (it.name === "tree_eq" && getTreeEqId() === -1) setTreeEqId(result.tree.id)
        recordItem("let", it.name)
        return
      }
      case "test": {
        compiledTestIndex++
        target.push({
          kind: "Test",
          lhs: compileExpr(it.lhs, lookupEntry, resolveUse, sinks),
          rhs: compileExpr(it.rhs, lookupEntry, resolveUse, sinks),
        })
        recordItem("test", undefined, compiledTestIndex)
        return
      }
      case "open": {
        const record = resolveExprRecord(it.expr, lookupEntry, resolveUse)
        if (!record || record.fields.length === 0)
          throw new Error("open: expression has no known record fields")
        const tree = record.fieldTrees ? undefined : compileExpr(it.expr, lookupEntry, resolveUse, sinks)
        const n = record.fields.length
        for (let i = 0; i < n; i++) {
          const fieldTree = record.fieldTrees ? record.fieldTrees[i] : applyTree(tree!, accTree(record.fields[i]), APPLY_BUDGET)
          const name = record.fields[i]
          const existing = stack[stack.length - 1].get(name)
          if (existing) {
            if (existing.tree?.id === fieldTree.id) continue
            throw new Error(`open: name '${name}' already in scope with different value`)
          }
          const fieldType = record.fieldTypes?.[i] ?? null
          // Propagate inner-field metadata if this field is itself
          // a record-typed binding (e.g., the kernel recq record).
          // Without this, `kernel.hyp_reduce` projection wouldn't work
          // across `open use` boundaries.
          const innerFields = (record as any).fieldInnerFields?.[i] as string[] | undefined
          const innerTrees = (record as any).fieldInnerTrees?.[i] as Tree[] | undefined
          define(name, { tree: fieldTree, type: fieldType, fields: innerFields, fieldTrees: innerTrees })
          if (isExport) {
            // Legacy mode: open re-exports opened names (with inner-field metadata).
            target.push({ kind: "Def", name, tree: fieldTree, type: fieldType, fields: innerFields, fieldTrees: innerTrees })
          }
        }
        recordItem("open")
        return
      }
    }
  }

  const items = parseItems(src)
  const hasFields = items.some(it => it.tag === "field")
  for (const it of items) runItem(it, decls, !hasFields)
  return decls
}
