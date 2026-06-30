// Bracket abstraction + driver. Compiles parsed AST (from parse.ts)
// to tree calculus via bracket abstraction, resolves scopes, inlines
// `use`d files, and produces Decl[].
//
// Sections:
//   5. Bracket abstraction (Expr → Cir → Tree)
//   6. Driver (scope stack, `use`, produces Decl[])

import { readFileSync } from "node:fs"
import { dirname, resolve as pathResolve } from "node:path"
import { type Tree, EagerSession, defaultSession } from "./eval/eager.js"
import type { Session, EvalStats, Budget } from "./eval/types.js"
import {
  parseItems,
  type Expr, type RecMember, type Param,
} from "./parse.js"

// The evaluator session the elaborator runs on. Set per-call by parseProgram
// (defaults to the eager defaultSession so helpers called outside a parse — e.g.
// stringToTree from tests — still work). Every tree operation goes through this,
// so an alternate backend (the Phase-3 naive backend, a WASM runtime) can drive
// elaboration. Handles stay typed `Tree` for now but are treated opaquely: built
// via cs.leaf/stem/fork, reduced via cs.apply, compared via cs.equal, inspected
// via cs.classify — never by direct field/.id access.
let cs: Session<Tree> = defaultSession
const B = (): Budget => ({ remaining: APPLY_BUDGET })

// ──────────────────────── 5. Bracket abstraction ─────────────────────────

// Step budget passed to applyTree from the compiler/elaborator. Large
// enough that elaboration of any well-formed program terminates; small
// enough that runaway evaluation aborts before exhausting host memory.
// (Raised 10M -> 40M when Pi/Record/Sigma unified onto the generic negative-
// telescope recognizer `neg_check`: the kernel self-check at load now costs
// ~16M steps vs ~8M for the old specialized recognizers — a more general
// recognizer, ~2x heavier per Pi-check; a validated lean fast path could
// reclaim it later, cf. the tree_eq native-fast-path discipline.)
const APPLY_BUDGET = 40_000_000

// Auto-verification cache (§ module checking): each module's typed exports are
// checked through the kernel once per process (file content is immutable, and a
// module's verdict is a pure function of its content + the fixed kernel). Keyed by
// absolute path. Without this, every file that opens the kernel would re-run the
// whole kernel self-check at compile time.
const verifiedModules = new Set<string>()

// Per-session module-record cache: memoize resolveUse(abs, raw) → its export record,
// so a module `use`d by many files (above all the kernel) is elaborated ONCE per
// session instead of re-traversed each time. Keyed by SESSION because the record
// holds Tree handles (arena indices) valid only within one session — a WeakMap so an
// independent per-file session's cache is GC'd with it, while a shared session
// accumulates the whole suite's modules (bounded, a few dozen). Within a session it
// is keyed by abs path (raw imports get a distinct key — `use raw` drops annotations
// and can yield a different tree than `use`).
const moduleCacheBySession = new WeakMap<Session<Tree>, Map<string, ScopeEntry>>()

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
    case "I":   return cs.fork(cs.fork(cs.leaf(), cs.leaf()), cs.leaf())
    case "K":   return cs.stem(cs.leaf())
    case "S":   return cs.fork(cs.stem(cs.fork(cs.leaf(), cs.leaf())), cs.leaf())
    case "lam": throw new Error(`cirToTree: unexpected lambda for ${e.x}`)
    case "app": {
      if (e.f.tag === "app" && e.f.f.tag === "S") return cs.fork(cs.stem(cirToTree(e.f.x)), cirToTree(e.x))
      // Full K application: K(x)(y) → x (drop second arg)
      if (e.f.tag === "app" && e.f.f.tag === "K") return cirToTree(e.f.x)
      if (e.f.tag === "K") return cs.fork(cs.leaf(), cirToTree(e.x))
      if (e.f.tag === "I") return cirToTree(e.x)
      // Partial S application: S(x) → stem(stem(x)) so that S(x)(y) = fork(stem(x), y)
      if (e.f.tag === "S") return cs.stem(cs.stem(cirToTree(e.x)))
      return cs.apply(cirToTree(e.f), cirToTree(e.x), B())
    }
  }
}

// Scope entry: a compiled tree plus optional MODULE export metadata (set by
// resolveUse, propagated through `let m = use "f"`). Needed because a module's
// fallback value is a Church-encoded record the runtime cut can't read; for
// everything else, projection is the §2.6 cut and needs no metadata.
interface ScopeEntry {
  tree?: Tree
  type?: Tree | null    // null = untyped, undefined = not yet set
  fields?: string[]
  fieldTrees?: Tree[]
  fieldTypes?: (Tree | null)[]  // per-field types for open
  params?: SigParam[]   // named-argument signature (leading binder params + defaults)
}

// Collect every evaluator handle the session holds ACROSS .disp file boundaries — the module
// cache's exported binding/type trees. This is the root set for rust-eager's scoped
// reclamation (Session.endScope): at a shared-session file boundary everything allocated in
// the scope is freed EXCEPT what's reachable from these. (`verifiedModules` holds only path
// strings; per-file `decls`/scope are dropped when `runFile` returns; the native `tree_eq`
// handle is rooted inside the arena; `SigParam.default` is an Expr, not a handle.) MUST stay
// in sync with any new cross-file handle holder — a missed root frees a live node (which the
// differential oracle catches: the suite goes red).
export function collectSessionRoots(session: Session<Tree>): Tree[] {
  const cache = moduleCacheBySession.get(session)
  if (!cache) return []
  const roots: Tree[] = []
  for (const e of cache.values()) {
    if (e.tree != null) roots.push(e.tree)
    if (e.type != null) roots.push(e.type)
    if (e.fieldTrees) for (const t of e.fieldTrees) if (t != null) roots.push(t)
    if (e.fieldTypes) for (const t of e.fieldTypes) if (t != null) roots.push(t)
  }
  return roots
}

// A binding's named-argument signature: the leading run of its value-lambda's
// (and/or type's) parameters, in declared order, each with its optional default
// recipe. Drives reorderable / default / partial named calls — see § named-arg
// resolution. `name` is the canonical position key; `default` (Expr | null) is
// the omitted-arg fallback.
type SigParam = { name: string; default: Expr | null }

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
      return e.fields.some(f => (f.type !== null && exprMentions(f.type, name)) ||
        ((f as any).value != null && exprMentions((f as any).value, name)))
    case "sumType":
      return e.variants.some(v => v.type !== null && exprMentions(v.type, name))
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
    case "if":
      return exprMentions(e.cond, name) || exprMentions(e.thenBody, name) || exprMentions(e.elseBody, name)
    case "match":
      return exprMentions(e.cond, name) ||
        e.arms.some(a => !a.binders.includes(name) && exprMentions(a.body, name))
  }
}

// Peel a left-leaning app chain into head + args.
function peelApp(e: Expr): { head: Expr; args: Expr[] } {
  const args: Expr[] = []
  let cur = e
  while (cur.tag === "app") { args.unshift(cur.x); cur = cur.f }
  return { head: cur, args }
}

// Rewrite `select_lazy ({_} -> A) ({_} -> B) cond` → `if cond then A else B`
// when both thunks have form `{_} -> body` (or `{x} -> body` with x unused).
// This avoids the cirToTree eager-K-body evaluation that fires on self-
// referential select_lazy uses (CLAUDE.md "Compiler workarounds"). The `if`
// desugaring (see "if" case below) wraps each branch in a closure over its
// free vars, side-stepping the K(body) construction whose body would
// otherwise be reduced eagerly at compile time.
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
  let rewritten: Expr = { tag: "if", cond, thenBody, elseBody }
  // Re-apply any trailing args (select-then-apply).
  for (let i = 3; i < args.length; i++) {
    rewritten = { tag: "app", f: rewritten, x: args[i] }
  }
  return rewritten
}

// ───────────────── Named / default / reorderable arguments ──────────────────
// A function signature is a "shopping list" of named requirements: a named call
// `f { name := val, … }` supplies them in ANY order, omitted ones fall back to
// their defaults, and a non-total supply yields a residual function awaiting the
// rest (partial application). The whole feature is a pure ELABORATION-TIME rewrite
// to the canonical positional form, so the load-bearing property is free:
// reorderings and default-fills produce the IDENTICAL tree as `f a b` (same
// rewritten AST → same tree, modulo the η-collapse the bracket abstractor already
// does). No kernel/runtime representation changes — the domain telescope (Pi cells)
// is untouched; the names live in a host-side per-binding signature.

// Collect the leading run of binder parameters across nested/curried binders.
// `{a, b} -> {c} -> body` and `{a} -> {b} -> {c} -> body` both yield [a,b,c].
// Stops at the first non-binder body (e.g. an `if`/`fix`/application head).
function peelBinderParams(e: Expr): Param[] {
  const out: Param[] = []
  let cur = e
  while (cur.tag === "binder") { out.push(...cur.params); cur = cur.body }
  return out
}

// Build a binding's named-argument signature from its value lambda (authoritative
// for NAMES — bracket abstraction binds exactly those) and, secondarily, its type
// annotation (a `{x:A} -> {y:B := d} -> R` Pi-binder, for doc-style defaults). A
// value param's own `:= d` wins; otherwise the type param at the same position
// supplies the default. Anonymous params (`_`) get a synthetic, un-supplyable name
// so partial application past them still works structurally. Returns undefined when
// the value isn't a leading-lambda (e.g. `fix (…)`), i.e. no addressable params.
function extractSignature(valueExpr: Expr, typeExpr: Expr | null | undefined): SigParam[] | undefined {
  const vparams = peelBinderParams(valueExpr)
  if (vparams.length === 0) return undefined
  const tparams = typeExpr ? peelBinderParams(typeExpr) : []
  const sig: SigParam[] = []
  for (let i = 0; i < vparams.length; i++) {
    const vp = vparams[i]
    const name = vp.name ?? `__arg${i}`
    const tdef = i < tparams.length ? (tparams[i].default ?? null) : null
    sig.push({ name, default: vp.default ?? tdef })
  }
  return sig.length > 0 ? sig : undefined
}

// Simultaneous capture-avoiding substitution over the surface AST. Used to fill a
// default recipe that references PRIOR parameters (`b := double a`) with the
// already-resolved argument for each prior. Binders / match arms / recValue
// members that rebind a name shadow it (conservative: a shadowed name is simply
// not substituted — safe for the closed default recipes this serves).
function substExpr(e: Expr, map: Map<string, Expr>): Expr {
  if (map.size === 0) return e
  const without = (names: (string | null)[]): Map<string, Expr> => {
    const live = names.filter((n): n is string => n !== null && map.has(n))
    if (live.length === 0) return map
    const m = new Map(map); for (const n of live) m.delete(n); return m
  }
  switch (e.tag) {
    case "leaf": case "num": case "str": case "hole": case "use": return e
    case "var": return map.get(e.name) ?? e
    case "app": return { tag: "app", f: substExpr(e.f, map), x: substExpr(e.x, map) }
    case "ann": return { tag: "ann", expr: substExpr(e.expr, map), type: substExpr(e.type, map) }
    case "proj": return { tag: "proj", target: substExpr(e.target, map), field: e.field }
    case "if": return { tag: "if", cond: substExpr(e.cond, map), thenBody: substExpr(e.thenBody, map), elseBody: substExpr(e.elseBody, map) }
    case "binder": {
      const inner = without(e.params.map(p => p.name))
      return { tag: "binder",
        params: e.params.map(p => ({ name: p.name, type: p.type ? substExpr(p.type, inner) : null,
          default: p.default ? substExpr(p.default, inner) : p.default })),
        body: substExpr(e.body, inner) }
    }
    case "recType":
      return { tag: "recType", fields: e.fields.map(f => ({ name: f.name,
        type: f.type ? substExpr(f.type, map) : null,
        value: f.value != null ? substExpr(f.value, map) : f.value })) }
    case "recValue":
      // Field values are substituted (their own names bind only for LATER fields,
      // which is beyond what defaults need); members keep their structure.
      return { tag: "recValue", fields: e.fields.map(f => ({ name: f.name, type: f.type, value: substExpr(f.value, map) })),
        members: e.members, trailing: e.trailing ? substExpr(e.trailing, map) : e.trailing }
    case "match":
      return { tag: "match", cond: substExpr(e.cond, map),
        arms: e.arms.map(a => ({ pat: a.pat, binders: a.binders, body: substExpr(a.body, without(a.binders)) })) }
  }
}

// Resolve a named call `head { fields }` against `head`'s tracked signature to the
// canonical positional/partial form. Returns null when this isn't a named call
// (so the caller falls back to ordinary record application): a field names a
// non-parameter, the record has members/trailing, or it's empty. Otherwise:
//   • each param, in DECLARED order, takes its supplied field, else its default
//     (with prior args substituted), else becomes an awaited binder (missing);
//   • the result is `head a0 a1 … an` wrapped in binders for the missing params.
// Missing params that are a trailing suffix η-collapse to plain currying via the
// bracket abstractor (`{n} -> f a n` → `f a`), so prefix partial application and
// full application share this one construction.
function resolveNamedCall(headExpr: Expr, rec: Extract<Expr, { tag: "recValue" }>, sig: SigParam[]): Expr | null {
  if ((rec.members && rec.members.length > 0) || rec.trailing) return null
  if (rec.fields.length === 0) return null
  const paramNames = new Set(sig.map(p => p.name))
  const supplied = new Map<string, Expr>()
  for (const f of rec.fields) {
    if (!paramNames.has(f.name)) return null  // not a named-arg call → ordinary application
    supplied.set(f.name, f.value)
  }
  const argExprs: Expr[] = []
  const missing: SigParam[] = []
  const priorMap = new Map<string, Expr>()   // prior param name → its resolved arg, for default substitution
  for (const p of sig) {
    let arg: Expr
    if (supplied.has(p.name)) arg = supplied.get(p.name)!
    else if (p.default !== null) arg = substExpr(p.default, priorMap)
    else { arg = { tag: "var", name: p.name }; missing.push(p) }
    argExprs.push(arg)
    priorMap.set(p.name, arg)
  }
  let body: Expr = headExpr
  for (const a of argExprs) body = { tag: "app", f: body, x: a }
  if (missing.length > 0)
    body = { tag: "binder", params: missing.map(p => ({ name: p.name, type: null })), body }
  return body
}

// Recognise `f { … } [extra…]` where `f`'s tracked signature has named params and
// the record's fields are a subset of them; rewrite to the canonical positional/
// partial form, re-applying any trailing positional args. Returns null otherwise
// (ordinary application — including record-domain functions, whose single param
// name won't match a multi-field record's names).
function tryNamedCall(e: Expr, lookupEntry: (name: string) => ScopeEntry | undefined): Expr | null {
  if (e.tag !== "app") return null
  const { head, args } = peelApp(e)
  if (head.tag !== "var" || args.length === 0) return null
  const sig = lookupEntry(head.name)?.params
  if (!sig) return null
  if (args[0].tag !== "recValue") return null
  const resolved = resolveNamedCall(head, args[0], sig)
  if (resolved === null) return null
  let out = resolved
  for (let i = 1; i < args.length; i++) out = { tag: "app", f: out, x: args[i] }
  return out
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
  resolveUse: (path: string, raw?: boolean) => ScopeEntry,
  sinks?: CompileSinks,
): Cir {
  const lookup = (name: string) => lookupEntry(name)?.tree
  switch (e.tag) {
    case "leaf": return { tag: "lit", t: cs.leaf() }
    case "num": {
      const zero = lookup("zero")
      const succ = lookup("succ")
      if (!zero || !succ) throw new Error(`numeric literal ${e.value}: zero and succ must be in scope`)
      let result = zero
      for (let i = 0; i < e.value; i++) {
        result = cs.apply(succ, result, B())
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
      // `if cond then A else B [args...]` so the recursive branch
      // bodies don't hit cirToTree's eager K-body reduction. See
      // tryRewriteSelectLazy for details.
      const rewritten = tryRewriteSelectLazy(e, lookupEntry)
      if (rewritten !== null) return exprToCir(rewritten, lookupEntry, resolveUse, sinks)
      // Named / default / reorderable arguments: if the head has a tracked
      // signature and the first arg is a matching record, rewrite to the
      // canonical positional/partial call (see § named-arg resolution).
      const named = tryNamedCall(e, lookupEntry)
      if (named !== null) return exprToCir(named, lookupEntry, resolveUse, sinks)
      return { tag: "app",
        f: exprToCir(e.f, lookupEntry, resolveUse, sinks),
        x: exprToCir(e.x, lookupEntry, resolveUse, sinks),
      }
    }
    case "ann": return exprToCir(e.expr, lookupEntry, resolveUse, sinks) // erase type
    case "binder": {
      // Shadow binder params so they don't resolve to scope entries.
      // (Projection on a bound variable is the runtime §2.6 cut — no
      // compile-time field metadata is needed.)
      const paramNames = new Set(e.params.map((p, i) => p.name ?? `_anon${i}`))
      const shadowedLookup = (name: string): ScopeEntry | undefined => {
        if (paramNames.has(name)) return undefined
        return lookupEntry(name)
      }

      let body = exprToCir(e.body, shadowedLookup, resolveUse, sinks)
      for (let i = e.params.length - 1; i >= 0; i--) {
        const name = e.params[i].name ?? `_anon${i}`
        body = { tag: "lam", x: name, body }
      }
      return body
    }
    case "recType": {
      // A record-type literal IS a telescope type: each field becomes a WAIT-FORM
      // cell — `proj_cell name ty`, or `deriv_cell name recipe` for a derived
      // `name := e` member — consed into `t cell (λname. rest)` and wrapped in
      // `Telescope`. Later fields' types and derived recipes compile under lams
      // binding the PRIOR field names, so `{ a : Nat, b := double a }` scopes
      // naturally. (Pi/Sigma emit the same cells, so surface and manual agree.)
      const TelescopeEntry = lookupEntry("Telescope")
      const projCellEntry = lookupEntry("proj_cell")
      const derivCellEntry = lookupEntry("deriv_cell")
      if (!TelescopeEntry?.tree || !projCellEntry?.tree || !derivCellEntry?.tree)
        throw new Error("record type literal '{ name : T }': 'Telescope', 'proj_cell', and 'deriv_cell' must be in scope (open the kernel prelude)")
      const leafCir: Cir = { tag: "lit", t: cs.leaf() }
      let teleCir: Cir = leafCir
      for (let i = e.fields.length - 1; i >= 0; i--) {
        const f = e.fields[i]
        const priorNames = new Set(e.fields.slice(0, i).map(p => p.name))
        const shadowed = (n: string): ScopeEntry | undefined =>
          priorNames.has(n) ? {} : lookupEntry(n)
        const nameCir: Cir = { tag: "lit", t: stringToTree(f.name) }
        // derived field `name := e` -> deriv_cell name recipe; else proj_cell name ty.
        // A field's type is a TYPE position: desugar binders to `Pi` (so `s : T -> T`
        // is a `Pi`, not a value-lambda), exactly as a binding annotation does.
        const entryCir: Cir = f.value != null
          ? cap(cap({ tag: "lit", t: derivCellEntry.tree }, nameCir),
                exprToCir(f.value, shadowed, resolveUse, sinks))
          : cap(cap({ tag: "lit", t: projCellEntry.tree }, nameCir),
                exprToCir(
                  lookupEntry("Pi")?.tree ? binderToPi(f.type ?? { tag: "var", name: "Tree" }) : (f.type ?? { tag: "var", name: "Tree" }),
                  shadowed, resolveUse, sinks))
        teleCir = cap(cap(leafCir, entryCir), { tag: "lam", x: f.name, body: teleCir })
      }
      return cap({ tag: "lit", t: TelescopeEntry.tree }, teleCir)
    }
    case "sumType": {
      // A sum-type literal `< Tag : T, … >` IS a `Coproduct` over a list of
      // variants — the DUAL of recType → Telescope. Each variant desugars to
      // `pair "Tag" [argTypes]`: `Tag : T` → `pair "Tag" [T]` (single-arg),
      // `Tag` → `pair "Tag" []` (nullary). The variant list and per-variant arg
      // list are leaf-based cons-chains (cons = `t h tl`, nil = leaf), matching
      // the `[…]` array sugar — so `< … >` is tree-identical to the hand-written
      // `Coproduct [pair "Tag" […]]`. A payload type compiles in TYPE position
      // (binderToPi), exactly like a recType field type. (Coproduct/pair must be
      // in scope, like recType needs Telescope.)
      const CoproductEntry = lookupEntry("Coproduct")
      const pairEntry = lookupEntry("pair")
      if (!CoproductEntry?.tree || !pairEntry?.tree)
        throw new Error("sum type literal '< Tag : T >': 'Coproduct' and 'pair' must be in scope (open the kernel prelude)")
      const leafCir: Cir = { tag: "lit", t: cs.leaf() }
      const consCir = (h: Cir, tl: Cir): Cir => cap(cap(leafCir, h), tl)
      const pairCir: Cir = { tag: "lit", t: pairEntry.tree }
      let variantsCir: Cir = leafCir
      for (let i = e.variants.length - 1; i >= 0; i--) {
        const v = e.variants[i]
        const nameCir: Cir = { tag: "lit", t: stringToTree(v.name) }
        // arg list: single-element [T] when `Tag : T`, else nullary [].
        const argsCir: Cir = v.type != null
          ? consCir(
              exprToCir(lookupEntry("Pi")?.tree ? binderToPi(v.type) : v.type, lookupEntry, resolveUse, sinks),
              leafCir)
          : leafCir
        variantsCir = consCir(cap(cap(pairCir, nameCir), argsCir), variantsCir)
      }
      return cap({ tag: "lit", t: CoproductEntry.tree }, variantsCir)
    }
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
            // Module metadata propagation (`let m = use "f"`) so an inline
            // `open m` still sees the export list.
            const record = resolveExprRecord(m.body, fieldLookup, resolveUse)
            rebind(m.name, { tree, fields: record?.fields, fieldTrees: record?.fieldTrees, fieldTypes: record?.fieldTypes })
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
              ?? recordFieldsFromTree(compileExpr(m.expr, fieldLookup, resolveUse, sinks), fieldLookup)
            if (!record || record.fields.length === 0)
              throw new Error("open (inline): expression has no known record fields")
            const targetTree = record.fieldTrees
              ? undefined
              : compileExpr(m.expr, fieldLookup, resolveUse, sinks)
            const n = record.fields.length
            for (let i = 0; i < n; i++) {
              const fieldTree = record.fieldTrees
                ? record.fieldTrees[i]
                : cs.apply(targetTree!, accTree(record.fields[i]), B())
              const fieldType = record.fieldTypes?.[i] ?? null
              rebind(record.fields[i], { tree: fieldTree, type: fieldType })
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
      // §2.6 record: {x := a; y := b} → mk_record ["x","y"] [list_const a, list_const b]
      // — a `prod` over a string-interned name header, read by name through the
      // cut. (mk_record/list_const must be in scope, like `match` needs `prod`.)
      const recordValEntry = lookupEntry("mk_record")
      const listConstEntry = lookupEntry("list_const")
      if (!recordValEntry?.tree || !listConstEntry?.tree)
        throw new Error("record literal '{ := }': 'mk_record' and 'list_const' must be in scope (open the kernel prelude)")
      const recordVal: Cir = { tag: "lit", t: recordValEntry.tree }
      const listConst: Cir = { tag: "lit", t: listConstEntry.tree }
      // names header: a closed cons-chain of string tags.
      let namesTree: Tree = cs.leaf()
      for (let i = e.fields.length - 1; i >= 0; i--)
        namesTree = cs.fork(stringToTree(e.fields[i].name), namesTree)
      // Sequential field scope (telescope discipline): later fields see earlier
      // ones by name ({ a := 2; b := double a }). The record core references
      // each field as a var; each field wraps the rest as ((λname. rest) value),
      // with PRIOR names shadowed while compiling `value` — so a field's value
      // compiles before its own name binds (`respond := respond` resolves
      // outward, field puns included).
      const consCir = (h: Cir, tl: Cir): Cir => cap(cap({ tag: "lit", t: cs.leaf() }, h), tl)
      let payloadCir: Cir = { tag: "lit", t: cs.leaf() }
      for (let i = e.fields.length - 1; i >= 0; i--)
        payloadCir = consCir(cap(listConst, { tag: "var", name: e.fields[i].name }), payloadCir)
      let result: Cir = cap(cap(recordVal, { tag: "lit", t: namesTree }), payloadCir)
      for (let i = e.fields.length - 1; i >= 0; i--) {
        const priorNames = new Set(e.fields.slice(0, i).map(f => f.name))
        const shadowedLookup = (n: string): ScopeEntry | undefined =>
          priorNames.has(n) ? {} : fieldLookup(n)
        const vc = exprToCir(e.fields[i].value, shadowedLookup, resolveUse, sinks)
        result = cap({ tag: "lam", x: e.fields[i].name, body: result }, vc)
      }
      return result
    }
    case "use": {
      // A file resolves to a module tuple { record, typ } (§2.6 records):
      //   record = a product of the file's exported values, keyed by name;
      //   typ    = `Record [(name, declaredType)…]` over the *annotated* exports.
      // Verification goes through the kernel's `verify` helper —
      // `verify (use "f")` = `param_apply (use "f").typ (use "f").record` — so it
      // runs UNDER the walker (parametricity guards apply to every export). It must
      // NOT be the raw juxtaposition `(use "f").typ (use "f").record`, which bypasses
      // the walker and would let a non-parametric export slip through. (Gradual:
      // unannotated exports are absent from `typ`, so skipped.) Falls back to the bare value record when
      // the cut/Record formers aren't in scope (e.g. files that don't open the
      // kernel — they carry no checkable annotations anyway). `open use` is
      // unaffected: it splices the export metadata, not this value.
      // `use raw "f"` skips the file's annotations (no `typ`); it falls through to
      // the bare value record below since `entry.fieldTypes` are all null.
      const entry = resolveUse(e.path, e.raw)
      const mk_record = lookupEntry("mk_record")?.tree
      const list_const = lookupEntry("list_const")?.tree
      const Record = lookupEntry("Record")?.tree
      if (!mk_record || !list_const || !Record || !entry.fields || !entry.fieldTrees)
        return { tag: "lit", t: entry.tree! }
      const consList = (items: Tree[]): Tree => items.reduceRight<Tree>((acc, h) => cs.fork(h, acc), cs.leaf())
      const constWrap = (v: Tree): Tree => cs.apply(list_const, v, B())
      const mkRecord = (names: string[], vals: Tree[]): Tree =>
        cs.apply(cs.apply(mk_record, consList(names.map(stringToTree)), B()), consList(vals.map(constWrap)), B())
      const names = entry.fields, vals = entry.fieldTrees, types = entry.fieldTypes ?? []
      const valuesRecord = mkRecord(names, vals)
      // typ = Record [ pair name type ]  over annotated exports (pair = fork(name,type))
      const typEntries: Tree[] = []
      for (let i = 0; i < names.length; i++)
        if (types[i]) typEntries.push(cs.fork(stringToTree(names[i]), types[i]!))
      const typ = cs.apply(Record, consList(typEntries), B())
      return { tag: "lit", t: mkRecord(["record", "typ"], [valuesRecord, typ]) }
    }
    case "proj": {
      // r.x is the §2.6 cut `r (acc x)`. When the target is a statically-known
      // record with the field, keep the compile-time collapse (return the field
      // tree); otherwise emit the runtime cut, so projection works on any product
      // value — a runtime metadata record, or a module tuple whose value carries
      // fields not in the binding's compile-time field list (e.g. `(use f).typ`,
      // where the let's known fields are the file's exports, not record/typ).
      const record = resolveExprRecord(e.target, lookupEntry, resolveUse)
      if (record && record.fieldTrees) {
        const idx = record.fields.indexOf(e.field)
        if (idx >= 0) return { tag: "lit", t: record.fieldTrees[idx] }
      }
      const target = exprToCir(e.target, lookupEntry, resolveUse, sinks)
      return cap(target, { tag: "lit", t: accTree(e.field) })
    }
    case "if": {
      // Desugar to closed-branch select-then-apply over `cond` (prelude):
      // `if c then e1 else e2` becomes
      // `cond c ({fvs...} -> e1) ({fvs...} -> e2) fv1 fv2 ... fvn`
      // where fvs is the union of free vars across both branches (only names
      // that aren't already closed top-level lits). Wrapping each branch in a
      // closure over its free vars defers evaluation (only the taken branch is
      // forced) AND keeps recursive bodies out of cirToTree's eager K-body
      // reduction (see tryRewriteSelectLazy / CLAUDE.md compiler workarounds).
      const condEntry = lookupEntry("cond")
      if (!condEntry?.tree)
        throw new Error("if: 'cond' must be in scope (import prelude)")

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
      const branchThen = wrap(thenCir)
      const branchElse = wrap(elseCir)

      // cond c branchThen branchElse fv1 ... fvn — the Scott Bool picks a branch
      // closure, the trailing fvs re-supply the free vars it abstracted over.
      let out: Cir = cap(cap(cap({ tag: "lit", t: condEntry.tree }, condCir), branchThen), branchElse)
      for (const v of fvs) out = cap(out, { tag: "var", name: v })
      return out
    }
    case "match": {
      // The §2.6 cut, with the WHOLE cut closed over the arms' free vars
      // (mirroring `if`'s branch closures):
      //   match c { A x => b1; … }
      //     ⟶  (λfv1…fvn. prod (pair ["A",…] [{x} -> b1, …]) c) fv1 … fvn
      // Closing keeps recursive calls in arm bodies open under bracket
      // abstraction — `self name x` in an arm is no longer a closed redex for
      // cirToTree's eager evaluation to unfold (the closed-prefix hazard;
      // CLAUDE.md § Compiler workarounds). The fvs close around the cut, NOT
      // as a row appended to the selected handler's result: kernel idioms rely
      // on a mis-tagged cut (e.g. a respond returning Err into hyp_reduce's
      // Extend/Reduce match) staying INERT — extra args applied to that junk
      // can re-enter recursion. Arm binders shadow the wrapper lams naturally.
      // With no free vars this is the plain cut, unchanged.
      if (!lookupEntry("prod")?.tree)
        throw new Error("match: 'prod' must be in scope (open the kernel prelude)")
      const condCir = exprToCir(e.cond, lookupEntry, resolveUse, sinks)
      const leafCir: Cir = { tag: "lit", t: cs.leaf() }

      // Compile each arm body with its binders shadowed.
      const arms = e.arms.map(a => {
        const bound = new Set(a.binders.filter(b => b !== "_"))
        const look = (n: string) => bound.has(n) ? undefined : lookupEntry(n)
        return { pat: a.pat, binders: a.binders, bound, bodyCir: exprToCir(a.body, look, resolveUse, sinks) }
      })

      // fv union over arms, each minus its own binders (arm order, then
      // discovery order — deterministic elaboration is load-bearing).
      const fvs: string[] = []
      const seen = new Set<string>()
      for (const a of arms) collectFreeVars(a.bodyCir, a.bound, fvs, seen)

      // A lam param that must bind nothing: deterministic fresh name.
      const freshFor = (base: string, body: Cir): string => {
        let n = base
        while (containsFree(body, n)) n += "_"
        return n
      }

      // The handler an arm contributes: the cut applies it to the payload
      // (annihilate: `(proj P tag) (pair_snd c)`); binders destructure it.
      //   0 binders → ignore the payload;  1 → the payload IS the binder;
      //   n≥2 → a right-nested pair (pair b0 (pair b1 …)), projected.
      const handlerCir = (a: typeof arms[number]): Cir => {
        const n = a.binders.length
        if (n <= 1) {
          const p = n === 1 && a.binders[0] !== "_"
            ? a.binders[0]
            : freshFor("__m", a.bodyCir)
          return { tag: "lam", x: p, body: a.bodyCir }
        }
        let inner: Cir = a.bodyCir
        for (let k = n - 1; k >= 0; k--) {
          const b = a.binders[k]
          inner = { tag: "lam", x: b === "_" ? freshFor(`__m${k}`, a.bodyCir) : b, body: inner }
        }
        const pairFst = exprToCir({ tag: "var", name: "pair_fst" }, lookupEntry, resolveUse, sinks)
        const pairSnd = exprToCir({ tag: "var", name: "pair_snd" }, lookupEntry, resolveUse, sinks)
        const pn = freshFor("__p", inner)
        let appd: Cir = inner
        for (let k = 0; k < n; k++) {
          let acc: Cir = { tag: "var", name: pn }
          for (let s = 0; s < k; s++) acc = cap(pairSnd, acc)   // pair_snd^k __p
          appd = cap(appd, k < n - 1 ? cap(pairFst, acc) : acc) // last binder takes the bare snd-chain
        }
        return { tag: "lam", x: pn, body: appd }
      }

      // Wildcard handler is appended PAST the names so an unmatched tag's
      // index_of (= the name count) lands on it.
      const named = arms.filter(a => a.pat !== "_")
      const wildcard = arms.find(a => a.pat === "_")
      let namesTree: Tree = cs.leaf()
      for (let i = named.length - 1; i >= 0; i--)
        namesTree = cs.fork(stringToTree(named[i].pat), namesTree)
      const handlerList = named.map(handlerCir)
      if (wildcard) handlerList.push(handlerCir(wildcard))
      let handlersCir: Cir = leafCir
      for (let i = handlerList.length - 1; i >= 0; i--)
        handlersCir = cap(cap(leafCir, handlerList[i]), handlersCir)

      const table = cap(cap(leafCir, { tag: "lit", t: namesTree }), handlersCir)
      const prodCir = exprToCir({ tag: "var", name: "prod" }, lookupEntry, resolveUse, sinks)
      let out: Cir = cap(cap(prodCir, table), condCir)
      for (let i = fvs.length - 1; i >= 0; i--) out = { tag: "lam", x: fvs[i], body: out }
      for (const v of fvs) out = cap(out, { tag: "var", name: v })
      return out
    }
  }
}

// Resolve MODULE export metadata known at compile time (for `open` splicing
// and projection on module values, whose fallback representation is a Church
// record the runtime cut can't read). Ordinary record values need none of
// this — projection on them is the §2.6 cut.
function resolveExprRecord(
  e: Expr,
  lookupEntry: (name: string) => ScopeEntry | undefined,
  resolveUse: (path: string, raw?: boolean) => ScopeEntry,
): { fields: string[]; fieldTrees?: Tree[]; fieldTypes?: (Tree | null)[] } | undefined {
  if (e.tag === "var") {
    const entry = lookupEntry(e.name)
    return entry?.fields ? { fields: entry.fields, fieldTrees: entry.fieldTrees, fieldTypes: entry.fieldTypes } : undefined
  }
  if (e.tag === "use") {
    const entry = resolveUse(e.path, e.raw)
    return entry.fields ? { fields: entry.fields, fieldTrees: entry.fieldTrees, fieldTypes: entry.fieldTypes } : undefined
  }
  return undefined
}

function compileExpr(
  e: Expr,
  lookupEntry: (name: string) => ScopeEntry | undefined,
  resolveUse: (path: string, raw?: boolean) => ScopeEntry,
  sinks?: CompileSinks,
): Tree {
  return cirToTree(eliminateLams(exprToCir(e, lookupEntry, resolveUse, sinks)))
}

// ──────────────────── 5b. Type construction ─────────────────────────────

// treePairFst(p): the left projection of a wait-form fork. A type is a
// wait-form; its left projection is the constant former-signature. Used only to
// recognise whether a binding's annotation IS the universe `Type`.
function treePairFst(p: Tree): Tree | null {
  const c = cs.classify!(p)
  return c.tag === "fork" ? c.left : null
}

// isUniverseTree(t, Type): is the annotation `t` EXACTLY the universe `Type`?
// Uses full tree identity, not the root signature: when `Type` is the strict universe
// (a Telescope-shaped wait-form), its signature is SHARED by every Pi/Telescope type, so
// a root-sig match would wrongly treat any `: <Pi-type>` binding as universe-typed and
// compile its value (e.g. Pi's `{A,B} -> …`) as a type. Exact identity matches only the
// universe itself. (Hash-consing makes this O(1); revisit if universe levels `Type i` land.)
function isUniverseTree(t: Tree, Type: Tree): boolean {
  return cs.equal!(t, Type)
}

// binderToPi(e): desugar a type-position binder into explicit `Pi` applications.
//   {n : A} -> B          ⟶  Pi A ({n} -> B)
//   {a : A, b : B} -> C   ⟶  Pi A ({a} -> Pi B ({b} -> C))
//   A -> B  (anonymous)   ⟶  Pi A ({_} -> B)
// The codomain is an *untyped* binder ({n} -> …), so it compiles by ordinary
// bracket abstraction — the dependent codomain `{n} -> Vec n` substitutes the
// argument for `n`. Domains and codomains are recursively desugared. Non-binders
// (Nat, `Eq Nat x y`, an explicit `Pi …` application) are returned unchanged:
// their tree already IS the type. This is the whole of "type construction" — no
// kernel hypotheses, no host-side check/infer. The desugar is annotation-free
// and produces trees bit-identical to the explicit `Pi` form (the sugar and
// `Pi A ({n} -> B)` are the same type); a binding's body is verified against its
// type by the in-language kernel, never here.
function binderToPi(e: Expr): Expr {
  if (e.tag !== "binder") return e
  const p = e.params[0]
  if (!p.type) throw new Error("Pi domain requires a type annotation")
  const dom = binderToPi(p.type)
  const rest: Expr = e.params.length > 1
    ? { tag: "binder", params: e.params.slice(1), body: e.body }
    : e.body
  const cod: Expr = { tag: "binder", params: [{ name: p.name, type: null }], body: binderToPi(rest) }
  return { tag: "app", f: { tag: "app", f: { tag: "var", name: "Pi" }, x: dom }, x: cod }
}

// compileType(e): compile an expression that denotes a TYPE to its tree. A
// type-position binder becomes a `Pi` (via binderToPi); every other shape is an
// ordinary expression whose tree IS the type. Falls back to plain compilation
// when `Pi` isn't in scope (non-kernel files carry no checkable types — a binder
// there is an ordinary value lambda).
function compileType(
  e: Expr,
  lookupEntry: (name: string) => ScopeEntry | undefined,
  resolveUse: (path: string, raw?: boolean) => ScopeEntry,
): Tree {
  const desugared = lookupEntry("Pi")?.tree ? binderToPi(e) : e
  return compileExpr(desugared, lookupEntry, resolveUse)
}

// ─────────────────────── 5c. Value/identifier encoding ───────────────────

// natLitTree(n): the lib's canonical Nat for `n` — zero = LEAF, succ(m) =
// `t t m` = fork(LEAF, m). Matches `succ`/`zero` applied in scope.
function natLitTree(n: number): Tree {
  let result: Tree = cs.leaf()
  for (let i = 0; i < n; i++) result = cs.fork(cs.leaf(), result)
  return result
}

// stringToTree(s): a string literal as a List of codepoint Nats (cons = fork,
// nil = LEAF) — bit-identical to the array literal `[c0, c1, …]` of its
// codepoints, so a string is a genuine String value and a deterministic,
// distinct tag per spelling. Reused to intern record field-name identifiers.
export function stringToTree(s: string): Tree {
  const codes = [...s].map(c => c.codePointAt(0)!)
  let result: Tree = cs.leaf()
  for (let i = codes.length - 1; i >= 0; i--) result = cs.fork(natLitTree(codes[i]), result)
  return result
}

// accTree(name): the §2.6 accessor for a field name — `acc name = inj name unit
// = fork(name, LEAF)`, with the name interned as a string tag. Applying a
// product (record) to it performs the cut and yields the named field.
function accTree(name: string): Tree {
  return cs.fork(stringToTree(name), cs.leaf())
}

// treeToNat / treeToString: decode the lib encodings (Nat = nested fork(LEAF,·),
// String = a cons-chain of codepoint Nats) back to host values — the inverse of
// natLitTree / stringToTree. Used to read a record's field-name header.
function treeToNat(t: Tree): number {
  let n = 0, cur = cs.classify!(t)
  while (cur.tag === "fork") { n++; cur = cs.classify!(cur.right) }
  return n
}
function treeToString(t: Tree): string {
  const codes: number[] = []
  let cur = cs.classify!(t)
  while (cur.tag === "fork") { codes.push(treeToNat(cur.left)); cur = cs.classify!(cur.right) }
  return codes.length ? String.fromCodePoint(...codes) : ""
}

// recordFieldsFromTree(tree): if `tree` is a §2.6 record VALUE (an annihilate-
// rooted product — e.g. the output of `mk_record` / `Enum`), read its field-name
// header and extract each field via the cut, so `open` works on a *computed*
// record, not only on statically-known `use` modules. Returns undefined for
// anything that isn't such a record — in particular library TYPES are
// recognizer-rooted (not annihilate-rooted), so they are correctly excluded.
// Needs the kernel in scope (`annihilate_sig`/`type_meta`/`pair_fst`); without
// it, returns undefined.
function recordFieldsFromTree(
  tree: Tree,
  lookupEntry: (name: string) => ScopeEntry | undefined,
): { fields: string[]; fieldTrees?: Tree[]; fieldTypes?: (Tree | null)[] } | undefined {
  const annihilateSig = lookupEntry("annihilate_sig")?.tree
  const typeMeta = lookupEntry("type_meta")?.tree
  const pairFst = lookupEntry("pair_fst")?.tree
  if (!annihilateSig || !typeMeta || !pairFst) return undefined
  const sig = treePairFst(tree)
  if (!sig || !cs.equal!(sig, annihilateSig)) return undefined
  // names = pair_fst (type_meta tree): a cons-chain of interned string tags.
  const namesTree = cs.apply(pairFst, cs.apply(typeMeta, tree, B()), B())
  const fields: string[] = []
  const fieldTrees: Tree[] = []
  let cur = cs.classify!(namesTree)
  while (cur.tag === "fork") {
    const name = treeToString(cur.left)
    fields.push(name)
    fieldTrees.push(cs.apply(tree, accTree(name), B()))
    cur = cs.classify!(cur.right)
  }
  return fields.length > 0 ? { fields, fieldTrees } : undefined
}

// ──────────────────────────── 6. Driver ─────────────────────────────────

export type Decl =
  | { kind: "Def"; name: string; tree: Tree; type?: Tree | null }
  | { kind: "Test"; lhs: Tree; rhs: Tree }

export type ParseItemStats = {
  kind: "let" | "test" | "open" | "field"
  name?: string
  testIndex?: number
  sourcePath?: string
  depth: number
  stats: EvalStats
}

export type ParseProgramOptions = {
  onItem?: (item: ParseItemStats) => void
  // The evaluator session to run elaboration + verification on (any backend).
  // Defaults to the eager defaultSession.
  session?: Session<Tree>
}

export function parseProgram(src: string, sourcePath?: string, options: ParseProgramOptions = {}): Decl[] {
  // Default to the shared eager defaultSession so callers that don't manage
  // sessions keep the single-global-session behavior — in particular the
  // elaborator-validation tests, which load the kernel here (recognizing
  // tree_eq) and then build trees on that same session. An explicit session
  // (run.ts, one per file; a non-eager backend) opts into its own state.
  const session = options.session ?? defaultSession
  const prev = cs
  cs = session
  try { return parseProgramBody(src, sourcePath, options) } finally { cs = prev }
}

function parseProgramBody(src: string, sourcePath: string | undefined, options: ParseProgramOptions): Decl[] {
  const stack: Map<string, ScopeEntry>[] = [new Map()]
  const decls: Decl[] = []
  const dirStack = [sourcePath ? dirname(pathResolve(sourcePath)) : process.cwd()]
  const sourceStack = [sourcePath ? pathResolve(sourcePath) : undefined]
  const loadedFiles = new Set<string>() // cycle detection
  let compiledTestIndex = 0
  // Deferred module verification: instead of forcing each module's `param_apply
  // typ record` verdict at its `use` site, build it LAZILY (a suspended computation,
  // via the backend's applyLazy) and force them ALL at once at the end of the parse.
  // Lazy is acceptance-equivalent here (the verdict is fully forced against `Ok TT`,
  // so confluence gives the same NF as eager), and the single end-of-parse force is
  // where work-sharing — and, later, parallel reduction — applies. `scheduled` dedups
  // a module across its many `use` sites; the global `verifiedModules` dedups across
  // files. DISP_EAGER_VERIFY=1 forces eager build (an A/B + debug escape hatch).
  const pendingVerifications: { abs: string; verdict: Tree; okTT: Tree }[] = []
  const scheduledForVerification = new Set<string>()
  // Lazy verification is OPT-IN (DISP_LAZY_VERIFY=1), eager by default. It is
  // acceptance-equivalent (the verdict is fully forced against `Ok TT`, so confluence
  // gives eager's NF — see project_eager_normative_is_scaffolding), but EMPIRICALLY
  // the M1 lazy reducer OOMs on kernel verification: it materializes every suspended
  // sub-application as a node and has no reachability GC, so the fully-demanded walk
  // allocates far more than eager's in-place reduction + apply-memo. Lazy verification
  // becomes viable once the lazy path gains GC (the deferred §Costs-of-δⁿ cost) or an
  // NF-level memo; until then eager is the measured winner. The deferred-batch below
  // is backend-agnostic and is the seam where a future parallel backend fans out.
  const lazyVerify = process.env.DISP_LAZY_VERIFY === "1"
  const vApply = (f: Tree, x: Tree): Tree => {
    const al = cs.applyLazy
    return lazyVerify && al ? al.call(cs, f, x) : cs.apply(f, x, B())
  }

  const lookupEntry = (name: string): ScopeEntry | undefined => {
    for (let i = stack.length - 1; i >= 0; i--) {
      const e = stack[i].get(name)
      if (e !== undefined) return e
    }
    return undefined
  }
  const define = (name: string, entry: ScopeEntry) => stack[stack.length - 1].set(name, entry)

  function resolveUse(path: string, raw = false): ScopeEntry {
    const abs = pathResolve(dirStack[dirStack.length - 1], path)
    // Module cache (per session): return the already-elaborated record if present.
    // Checked BEFORE cycle detection on purpose — a cached module is fully loaded, so
    // returning it can't mask a real cycle (a module still mid-load is not yet cached,
    // so it still trips the loadedFiles guard below).
    let modCache = moduleCacheBySession.get(cs)
    if (!modCache) { modCache = new Map(); moduleCacheBySession.set(cs, modCache) }
    const cacheKey = raw ? `${abs}\0raw` : abs
    const hit = modCache.get(cacheKey)
    if (hit) return hit
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
    // Kernel formers needed to auto-verify this module, captured while its own scope
    // is still on the stack (popped in `finally`). Null if the kernel isn't in scope
    // (a file with no checkable annotations) — verification is then skipped.
    let vFormers: { paramApply: Tree; Record: Tree; mkRecord: Tree; listConst: Tree; ok: Tree; tt: Tree } | null = null
    try {
      for (const it of items) {
        runItem(it, fileDecls, !hasFields, raw)
      }
      const pa = lookupEntry("param_apply")?.tree, rec = lookupEntry("Record")?.tree
      const mkr = lookupEntry("mk_record")?.tree, lc = lookupEntry("list_const")?.tree
      const ok = lookupEntry("Ok")?.tree, tt = lookupEntry("TT")?.tree
      if (pa && rec && mkr && lc && ok && tt) vFormers = { paramApply: pa, Record: rec, mkRecord: mkr, listConst: lc, ok, tt }
    } finally {
      const fileScope = stack.pop()!
      dirStack.pop()
      sourceStack.pop()
      loadedFiles.delete(abs)
    }
    // Collect the file's top-level defs as a record.
    const fieldNames: string[] = []
    const fieldTrees: Tree[] = []
    const fieldTypes: (Tree | null)[] = []
    for (const d of fileDecls) {
      if (d.kind === "Def") {
        fieldNames.push(d.name)
        fieldTrees.push(d.tree)
        fieldTypes.push(d.type ?? null)
      }
    }
    // Auto-verification: check the module's typed exports through the kernel
    // (`verify mod` = `param_apply typ record`), so an export that doesn't inhabit
    // its declared type is a COMPILE error rather than a silent advisory. Runs under
    // the walker (param_apply), so the parametricity guards apply. Skipped for raw
    // imports (annotations dropped) and for files without the kernel in scope (no
    // checkable annotations). The scope is already popped, so a throw here is clean.
    if (!raw && vFormers && !verifiedModules.has(abs) && !scheduledForVerification.has(abs)) {
      const typEntries: Tree[] = []
      for (let i = 0; i < fieldNames.length; i++)
        if (fieldTypes[i]) typEntries.push(cs.fork(stringToTree(fieldNames[i]), fieldTypes[i]!))
      if (typEntries.length > 0) {
        const consList = (xs: Tree[]): Tree => xs.reduceRight<Tree>((acc, h) => cs.fork(h, acc), cs.leaf())
        const recordVal = cs.apply(cs.apply(vFormers.mkRecord, consList(fieldNames.map(stringToTree)), B()),
                                   consList(fieldTrees.map(v => cs.apply(vFormers!.listConst, v, B()))), B())
        const typVal = cs.apply(vFormers.Record, consList(typEntries), B())
        // Build the verdict (lazily by default — vApply); defer the force to the
        // single end-of-parse batch (see pendingVerifications). Keeps the elegant
        // whole-record form; the parallelism/laziness transfers to the final force.
        const verdict = vApply(vApply(vFormers.paramApply, typVal), recordVal)
        const okTT = cs.apply(vFormers.ok, vFormers.tt, B())
        pendingVerifications.push({ abs, verdict, okTT })
        scheduledForVerification.add(abs)
      }
    }
    // Church-encode: \sel. sel v1 v2 ... vn
    const n = fieldTrees.length
    if (n === 0) {
      const empty: ScopeEntry = { tree: cs.leaf(), fields: [] }
      modCache.set(cacheKey, empty)
      return empty
    }
    // Build as Cir, then compile
    const selName = "__use_sel"
    let body: Cir = { tag: "var", name: selName }
    for (const ft of fieldTrees) body = cap(body, { tag: "lit", t: ft })
    const cir: Cir = { tag: "lam", x: selName, body }
    const tree = cirToTree(eliminateLams(cir))
    const record: ScopeEntry = { tree, fields: fieldNames, fieldTrees, fieldTypes }
    modCache.set(cacheKey, record)
    return record
  }

  function recordItem(kind: "let" | "test" | "open" | "field", name?: string, testIndex?: number): void {
    options.onItem?.({
      kind,
      name,
      testIndex,
      sourcePath: sourceStack[sourceStack.length - 1],
      depth: sourceStack.length - 1,
      stats: cs.stats!(),
    })
  }

  function compileBinding(
    name: string,
    type: Expr | null | undefined,
    body: Expr,
    sinks?: CompileSinks,
    raw = false,
  ): { tree: Tree; type?: Tree | null; fields?: string[]; fieldTrees?: Tree[] } {
    let tree: Tree, inferredType: Tree | null = null
    const Type = lookupEntry("Type")?.tree

    // `raw` (a `use raw "…"` import) drops annotation *verification*: the
    // declared type is discarded and the body compiled directly. This breaks
    // the bootstrap cycle — a file's own annotations can reference names the
    // file is still defining — at the cost of the module's `typ`.
    // recType annotations participate once `Telescope` exists (they compile to
    // telescope types); without it they keep the legacy fields-metadata-only role.
    const isTypeAlias = type?.tag === "var" && type.name === "Type" && !!lookupEntry("Pi")?.tree
    if (!raw && type != null && (type.tag !== "recType" || lookupEntry("Telescope")?.tree) && (Type || lookupEntry("Pi")?.tree)) {
      // Build the annotation's type tree (binder → Pi, else plain compile). If the
      // annotation is the universe `Type`, the BODY itself denotes a type, so
      // compile it the same way; otherwise the body is a value (plain bracket
      // abstraction). Verifying the value inhabits the type is the in-language
      // kernel's job (`param_apply T body`, via the module tuple at the use site),
      // never the host's — there is no check/infer here.
      const type_tree = compileType(type, lookupEntry, resolveUse)
      tree = (Type && isUniverseTree(type_tree, Type))
        ? compileType(body, lookupEntry, resolveUse)
        : compileExpr(body, lookupEntry, resolveUse, sinks)
      inferredType = type_tree
    } else if (raw && isTypeAlias) {
      // Raw load of a `: Type := …` alias: still run the type-mode desugar
      // (binderToPi) on the body, so a `: Type := <arrow>` alias compiles to the
      // same `Pi` tree raw OR checked (mode-independent). This is the SYNTACTIC
      // `Type`-position decision only — pure desugar, no verification — so it
      // does NOT reopen the bootstrap cycle, yet lets raw-loaded kernel files use
      // `A -> B` for type aliases instead of the function-application `Arrow`.
      tree = compileType(body, lookupEntry, resolveUse)
    } else {
      // Untyped, raw value, or no kernel — plain value compilation.
      tree = compileExpr(body, lookupEntry, resolveUse, sinks)
    }

    // Module metadata propagation (`let m = use "f"`): `open m` and projection
    // on the module's Church-record fallback need the export list. Falls back to
    // reading the field header off the compiled tree, so a binding whose value is
    // a §2.6 record (`E := Enum {…}`, any `mk_record` result) is `open`-able and
    // projects at compile time.
    const record = resolveExprRecord(body, lookupEntry, resolveUse) ?? recordFieldsFromTree(tree, lookupEntry)
    // Track the named-argument signature (leading lambda params + defaults) so a
    // later `name { … }` call resolves by field name. Pure metadata — the tree is
    // unchanged. Skipped for raw imports (their bodies bypass annotations anyway).
    const params = extractSignature(body, type)
    define(name, { tree, type: inferredType, fields: record?.fields, fieldTrees: record?.fieldTrees, fieldTypes: record?.fieldTypes, params })
    return { tree, type: inferredType }
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

  function runItem(it: RecMember, target: Decl[], isExport: boolean, raw = false): void {
    const sinks = makeSinks(target)
    switch (it.tag) {
      case "field": {
        const result = compileBinding(it.name, it.type, it.value, sinks, raw)
        target.push({ kind: "Def", name: it.name, tree: result.tree, type: result.type })
        // Register the canonical tree_eq tree id with the runtime fast-path on first definition.
        if (it.name === "tree_eq") cs.recognizeNative?.("tree_eq", result.tree)
        recordItem("field", it.name)
        return
      }
      case "let": {
        const result = compileBinding(it.name, it.type, it.body, sinks, raw)
        if (isExport) {
          // Legacy mode: top-level let exports (for files not yet migrated)
          target.push({ kind: "Def", name: it.name, tree: result.tree, type: result.type })
        }
        if (it.name === "tree_eq") cs.recognizeNative?.("tree_eq", result.tree)
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
          ?? recordFieldsFromTree(compileExpr(it.expr, lookupEntry, resolveUse, sinks), lookupEntry)
        if (!record || record.fields.length === 0)
          throw new Error("open: expression has no known record fields")
        const tree = record.fieldTrees ? undefined : compileExpr(it.expr, lookupEntry, resolveUse, sinks)
        const n = record.fields.length
        for (let i = 0; i < n; i++) {
          const fieldTree = record.fieldTrees ? record.fieldTrees[i] : cs.apply(tree!, accTree(record.fields[i]), B())
          const name = record.fields[i]
          const existing = stack[stack.length - 1].get(name)
          if (existing) {
            if (existing.tree && cs.equal!(existing.tree, fieldTree)) continue
            throw new Error(`open: name '${name}' already in scope with different value`)
          }
          const fieldType = record.fieldTypes?.[i] ?? null
          define(name, { tree: fieldTree, type: fieldType })
          if (isExport) {
            // Legacy mode: open re-exports opened names.
            target.push({ kind: "Def", name, tree: fieldTree, type: fieldType })
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
  // Force ALL deferred module verifications at once. On a lazy backend this is where
  // the kernel verification walks actually run — one end-of-parse batch over a shared
  // hash-consed arena, so identical sub-checks across modules share work (and a future
  // parallel backend can fan the independent verdicts out). `equal` forces each verdict
  // to NF and compares to `Ok TT` — a closed force, so the result equals eager's.
  for (const { abs, verdict, okTT } of pendingVerifications) {
    if (!cs.equal!(verdict, okTT))
      throw new Error(`type check failed for module ${abs}: an export does not inhabit its declared type (verify returned non-(Ok TT))`)
    verifiedModules.add(abs)
  }
  return decls
}
