// Surface-AST rewrites (Expr -> Expr, all elaboration-time): the select_lazy->if
// rewrite, named/default/reorderable argument resolution, and the type-position
// binder->Pi desugar. Pure AST work — no session, no trees.

import type { Expr, Param } from "../parse.js"
import type { ScopeEntry, SigParam } from "./state.js"

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
export function exprMentions(e: Expr, name: string): boolean {
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
            if (m.value && exprMentions(m.value, name)) return true
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

// Peel the `test` equation marker off an lhs: `test e1 e2 …` → `e1 e2 …`.
// Pure AST surgery; returns lhs unchanged when it isn't marker-led. Whether to
// peel (marker pristine/unbound) is the caller's session-aware decision — see
// equationLhs in expr.ts.
export function peelTestMarker(lhs: Expr): Expr {
  let hd: Expr = lhs
  const args: Expr[] = []
  while (hd.tag === "app") { args.unshift(hd.x); hd = hd.f }
  if (!(hd.tag === "var" && hd.name === "test") || args.length === 0) return lhs
  return args.slice(1).reduce<Expr>((f, x) => ({ tag: "app", f, x }), args[0])
}

// Peel a left-leaning app chain into head + args.
export function peelApp(e: Expr): { head: Expr; args: Expr[] } {
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
export function tryRewriteSelectLazy(
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
export function extractSignature(valueExpr: Expr, typeExpr: Expr | null | undefined): SigParam[] | undefined {
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
    case "sumType":
      // (pre-split latent gap: this case was missing, so a default recipe
      // containing a sum-type literal skipped prior-param substitution)
      return { tag: "sumType", variants: e.variants.map(v => ({ name: v.name,
        type: v.type ? substExpr(v.type, map) : null })) }
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
export function tryNamedCall(e: Expr, lookupEntry: (name: string) => ScopeEntry | undefined): Expr | null {
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
export function binderToPi(e: Expr): Expr {
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
