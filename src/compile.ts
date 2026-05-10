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
  setNativeDispatcherTreeId, getNativeDispatcherTreeId, setNativeKernelSig, setNativeICanonicalId,
  SCOTT_TT,
} from "./tree.js"
import {
  parseItems,
  type Expr, type Param, type TypedField, type NamedField, type RecMember, type Item, type Tok,
} from "./parse.js"

// ──────────────────────── 5. Bracket abstraction ─────────────────────────

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
    case "app": {
      if (e.f.tag === "app" && e.f.f.tag === "S") return fork(stem(cirToTree(e.f.x)), cirToTree(e.x))
      // Full K application: K(x)(y) → x (drop second arg)
      if (e.f.tag === "app" && e.f.f.tag === "K") return cirToTree(e.f.x)
      if (e.f.tag === "K") return fork(LEAF, cirToTree(e.x))
      if (e.f.tag === "I") return cirToTree(e.x)
      // Partial S application: S(x) → stem(stem(x)) so that S(x)(y) = fork(stem(x), y)
      if (e.f.tag === "S") return stem(stem(cirToTree(e.x)))
      return applyTree(cirToTree(e.f), cirToTree(e.x), 10_000_000)
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
}

// Expr → Cir, with scope lookup and use-resolution.
function exprToCir(
  e: Expr,
  lookupEntry: (name: string) => ScopeEntry | undefined,
  resolveUse: (path: string) => ScopeEntry,
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
        result = applyTree(succ, result, 10_000_000)
      }
      return { tag: "lit", t: result }
    }
    case "var": {
      const entry = lookupEntry(e.name)
      return entry?.tree ? { tag: "lit", t: entry.tree } : { tag: "var", name: e.name }
    }
    case "hole": throw new Error("hole '_' cannot appear in untyped compilation")
    case "app":
      return { tag: "app",
        f: exprToCir(e.f, lookupEntry, resolveUse),
        x: exprToCir(e.x, lookupEntry, resolveUse),
      }
    case "ann": return exprToCir(e.expr, lookupEntry, resolveUse) // erase type
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

      let body = exprToCir(e.body, shadowedLookup, resolveUse)
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
      let fieldLookup = lookupEntry
      if (e.members && e.members.length > 0) {
        const localScope = new Map<string, ScopeEntry>()
        for (const m of e.members) {
          if (m.tag === "let") {
            const tree = compileExpr(m.body, fieldLookup, resolveUse)
            let fields: string[] | undefined, fieldTrees: Tree[] | undefined
            if (m.type?.tag === "recType") {
              fields = (m.type as any).fields.map((f: any) => f.name)
            } else {
              const record = resolveExprRecord(m.body, fieldLookup, resolveUse)
              fields = record?.fields; fieldTrees = record?.fieldTrees
            }
            localScope.set(m.name, { tree, fields, fieldTrees })
            const prevLookup = fieldLookup
            fieldLookup = (name: string) => localScope.get(name) ?? prevLookup(name)
          }
          // test/open in inline recValues: skip for now (tests need driver context)
        }
      }
      // Church encoding: {x := a; y := b} → \sel. sel a b
      const fieldCirs = e.fields.map(f => exprToCir(f.value, fieldLookup, resolveUse))
      const selName = "__sel"
      let body: Cir = { tag: "var", name: selName }
      for (const fc of fieldCirs) body = cap(body, fc)
      return { tag: "lam", x: selName, body }
    }
    case "use": {
      const entry = resolveUse(e.path)
      return { tag: "lit", t: entry.tree! }
    }
    case "proj": {
      // Compile target, then look up field index from target's field metadata.
      // Target must be a known record (var with fields, or use expression).
      const record = resolveExprRecord(e.target, lookupEntry, resolveUse)
      if (!record)
        throw new Error(`projection '.${e.field}': target has no known record fields`)
      const idx = record.fields.indexOf(e.field)
      if (idx < 0)
        throw new Error(`projection '.${e.field}': field not found (available: ${record.fields.join(", ")})`)
      if (record.fieldTrees) return { tag: "lit", t: record.fieldTrees[idx] }
      const target = exprToCir(e.target, lookupEntry, resolveUse)
      const sel = buildSelector(record.fields.length, idx)
      return cap(target, sel)
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

      const condCir = exprToCir(e.cond, lookupEntry, resolveUse)
      const thenCir = exprToCir(e.thenBody, lookupEntry, resolveUse)
      const elseCir = exprToCir(e.elseBody, lookupEntry, resolveUse)

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
): { fields: string[]; fieldTrees?: Tree[]; fieldTypes?: (Tree | null)[] } | undefined {
  if (e.tag === "var") {
    const entry = lookupEntry(e.name)
    return entry?.fields ? { fields: entry.fields, fieldTrees: entry.fieldTrees, fieldTypes: entry.fieldTypes } : undefined
  }
  if (e.tag === "use") {
    const entry = resolveUse(e.path)
    return entry.fields ? { fields: entry.fields, fieldTrees: entry.fieldTrees, fieldTypes: entry.fieldTypes } : undefined
  }
  // Application where the argument is a binder returning a recValue:
  // e.g. fix({ks : {...}} -> { f1 := ...; f2 := ... }) → fields from the recValue.
  // This propagates field metadata through higher-order patterns like fix.
  if (e.tag === "app" && e.x.tag === "binder" && e.x.body.tag === "recValue") {
    return { fields: e.x.body.fields.map(f => f.name) }
  }
  if (e.tag === "recValue") {
    return {
      fields: e.fields.map(f => f.name),
      fieldTrees: e.fields.map(f => compileExpr(f.value, lookupEntry, resolveUse)),
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
): Tree {
  return cirToTree(eliminateLams(exprToCir(e, lookupEntry, resolveUse)))
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
  const waitResult = applyTree(applyTree(checker, LEAF, 10_000_000), LEAF, 10_000_000)
  const sig = treePairFst(waitResult)
  if (!sig) throw new Error("checkerSig: expected fork from wait(checker)(t)")
  return sig
}

// Kernel query helpers. All require a trusted table populated with kernel refs.
// When trusted table is empty, all return false/null — elaborator falls back to untyped.

interface KernelHelpers {
  isUniverse(t: Tree): boolean
  isPi(t: Tree): boolean
  isNeutral(t: Tree): boolean
  piDomain(t: Tree): Tree
  piCodFn(t: Tree): Tree
  makeHyp(type: Tree, id: Tree): Tree
  univRank(t: Tree): Tree   // extract rank from a Type
  makeType(rank: Tree): Tree // construct Type(rank)
}

function makeKernelHelpers(trusted: Map<string, Tree>): KernelHelpers | null {
  // Derive signatures from trusted type constructors. After session 2 of
  // the soundness fix, public Pi/Type/Eq are wrapped in `guard`, so the
  // outer pair_fst of Pi(…) and Type(…) and Eq(…) all return the SAME
  // guard signature. To recover the actual checker signatures we must
  // peel the guard layer first via type_meta, then take pair_fst of the
  // inner core wait-value.
  const Pi = trusted.get("Pi")
  const Type = trusted.get("Type")
  const make_hyp = trusted.get("Hyp") ?? trusted.get("make_hyp")

  if (!Pi && !Type && !make_hyp) return null

  // Sample-and-peel: build a sample, peel guard if present, then pair_fst
  // gives the underlying checker signature.
  const I_FUNC = I_TREE
  const samplePi = Pi ? applyTree(applyTree(Pi, LEAF, 10_000_000), I_FUNC, 10_000_000) : null
  const sampleType = Type ? applyTree(Type, LEAF, 10_000_000) : null
  const sampleHyp = make_hyp ? applyTree(applyTree(make_hyp, LEAF, 10_000_000), LEAF, 10_000_000) : null

  // Outer signature = guard signature (same for Pi/Type after guarding).
  // Inner signature (after peeling type_meta) = the actual checker.
  const guardSig = samplePi ? treePairFst(samplePi) : (sampleType ? treePairFst(sampleType) : null)
  const piSigInner = samplePi ? treePairFst(typeMetaTree(samplePi)!) : null
  const typeSigInner = sampleType ? treePairFst(typeMetaTree(sampleType)!) : null
  const hypSig = sampleHyp ? treePairFst(sampleHyp) : null

  function hasSig(sig: Tree | null, t: Tree): boolean {
    if (!sig) return false
    const tSig = treePairFst(t)
    return tSig !== null && treeEqual(tSig, sig)
  }
  // Look through guard: if t is guarded, peel; otherwise return t.
  function unguardOrSelf(t: Tree): Tree {
    if (guardSig && hasSig(guardSig, t)) {
      const inner = typeMetaTree(t)
      return inner ?? t
    }
    return t
  }

  return {
    isUniverse(t) { return hasSig(typeSigInner, unguardOrSelf(t)) },
    isPi(t) { return hasSig(piSigInner, unguardOrSelf(t)) },
    isNeutral(t) { return hasSig(hypSig, t) },
    // piDomain / piCodFn return the *unguarded* (core) domain and the
    // *unapplied* core-returning codFn closure. This intentionally differs
    // from the disp-side reflection helpers `pi_dom` / `pi_cod_fn` (which
    // re-guard / apply-and-re-guard for user-facing code). The elaborator
    // wants the internal forms because:
    //   - it threads results back into kernel-style raw application
    //     (`applyTree(codFn, x_tree, ...)`), which matches how q_pi_fn uses
    //     pi_meta_cod_fn internally — both produce core/unguarded results;
    //   - reflection helpers (isPi/isUniverse/isNeutral) all unguard before
    //     comparing signatures, so a guarded vs unguarded result_type
    //     produces the same answers downstream.
    // Keep these aligned with q_pi_fn's internal convention, NOT with the
    // user-facing disp reflection signature.
    piDomain(t) {
      const inner = unguardOrSelf(t)
      const meta = typeMetaTree(inner)
      if (!meta) throw new Error("piDomain: not a valid type tree")
      const d = treePairFst(meta)
      if (!d) throw new Error("piDomain: metadata not a pair")
      return d
    },
    piCodFn(t) {
      const inner = unguardOrSelf(t)
      const meta = typeMetaTree(inner)
      if (!meta) throw new Error("piCodFn: not a valid type tree")
      const c = treePairSnd(meta)
      if (!c) throw new Error("piCodFn: metadata not a pair")
      return c
    },
    makeHyp(type, id) {
      if (!make_hyp) throw new Error("makeHyp: make_hyp not trusted")
      return applyTree(applyTree(make_hyp, type, 10_000_000), id, 10_000_000)
    },
    univRank(t) {
      const inner = unguardOrSelf(t)
      const meta = typeMetaTree(inner)
      if (!meta) throw new Error("univRank: not a valid type tree")
      return meta
    },
    makeType(rank) {
      if (!Type) throw new Error("makeType: Type not trusted")
      return applyTree(Type, rank, 10_000_000)
    },
  }
}

// Native dispatcher / kernel-signature registration anchors.
// kernel.disp exports a small block of `:=` constants whose values are
// the canonical signatures the host needs to recognise. When we see one
// of those names being defined, we forward the resulting tree id to
// tree.ts. Once the dispatcher tree id and at least the hyp_reduce
// signature are registered, the native fast-path activates; until then
// it is dormant and apply runs the in-language stub.
// Dispatcher list (per spec §6.1): only kernel-primitive type-formers
// and the unguard handler. Bool/Nat/Eq predicates are no longer
// routed; their applications fall through to the walker. The
// handlers remain in the recq record (q_core_type_fn uses them
// internally for is_registered) but they're not part of the
// dispatcher's signature recognition list.
const NATIVE_SIG_NAMES = new Set([
  "kernel_hyp_reduce_sig",
  "kernel_guard_sig",
  "kernel_pi_sig",
  "kernel_core_type_sig",
  "kernel_guarded_type_sig",
  "kernel_bool_rec_sig",
  "kernel_nat_rec_sig",
  "kernel_eq_J_sig",
  "kernel_unguard_sig",
])
function registerNativeDispatcherAnchor(name: string, tree: Tree): void {
  if (name === "checked_apply" && getNativeDispatcherTreeId() === -1) {
    setNativeDispatcherTreeId(tree.id)
    return
  }
  if (name === "kernel_I_canonical") {
    setNativeICanonicalId(tree.id)
    return
  }
  if (NATIVE_SIG_NAMES.has(name)) {
    // Strip "kernel_" prefix and trailing "_sig" to get the handler key.
    const key = name.slice("kernel_".length, name.length - "_sig".length)
    setNativeKernelSig(key, tree.id)
  }
}

// Nat-level max: compare two nat trees. Since nats are Church-like with
// zero = LEAF, succ(n) = fork(TT, n), we can compare structurally.
function natTreeToNum(t: Tree): number {
  let n = 0
  let cur = t
  while (cur.tag === "fork") { n++; cur = cur.right }
  return n
}
function natMax(a: Tree, b: Tree): Tree {
  return natTreeToNum(a) >= natTreeToNum(b) ? a : b
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

  // Binder → Pi type construction. Infer domain's universe, build Pi.
  if (e.tag === "binder") {
    const Type = ctx.trusted.get("Type")
    const Pi = ctx.trusted.get("Pi")
    if (!Type || !Pi) {
      const tree = compileExpr(e, ctx.lookupEntry, ctx.resolveUse)
      return { tree, universe: null }
    }

    // Process one param, recursing for the rest.
    const param = e.params[0]
    if (!param.type) throw new Error("Pi domain requires a type annotation")
    const paramName = param.name ?? `_anon0`

    // Infer the domain's type to get its universe level
    const { tree: A_tree, universe: A_univ } = checkAsType(param.type, ctx)
    const A_level = A_univ && k.isUniverse(A_univ) ? k.univRank(A_univ) : LEAF // default 0

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
    let B_univ: Tree | null
    try {
      const checked = checkAsType(innerExpr, ctx)
      B_tree = checked.tree
      B_univ = checked.universe
    } finally {
      ctx.lookupEntry = prevLookup
      ctx.scopeDepth--
    }
    const B_level = B_univ && k.isUniverse(B_univ) ? k.univRank(B_univ) : LEAF

    // Pi level = max(A_level, B_level)
    const piLevel = natMax(A_level, B_level)

    const codFn = abstractTree(hyp, B_tree)

    const piTree = applyTree(applyTree(Pi, A_tree, 10_000_000), codFn, 10_000_000)
    const piUniv = k.makeType(piLevel)
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

type ElabCtx = {
  lookupEntry: (name: string) => ScopeEntry | undefined
  resolveUse: (path: string) => ScopeEntry
  kernel: KernelHelpers | null
  trusted: Map<string, Tree>
  scopeDepth: number
  debugTypeCheck: boolean
}

// Defense-in-depth: verify the kernel agrees with the elaborator's type dispatch.
// Gated behind debugTypeCheck — never on in production.
function assertTypeCheck(tree: Tree, expected: Tree | null, ctx: ElabCtx): Tree {
  if (ctx.debugTypeCheck && expected !== null) {
    const ok = applyTree(expected, tree, 10_000_000)
    if (!treeEqual(ok, TT)) {
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
    const ok = applyTree(expected, tree, 10_000_000)
    if (!treeEqual(ok, TT))
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
      const result_type = applyTree(codFn, hyp, 10_000_000)

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
  const ok = applyTree(expected, tree, 10_000_000)
  if (!treeEqual(ok, TT))
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
        const result_tree = applyTree(f_tree, x_tree, 10_000_000)
        const result_type = applyTree(codFn, x_tree, 10_000_000)
        return { tree: result_tree, type: result_type }
      }

      // Untyped fallback
      const x_tree = check(e.x, null, ctx)
      const result_tree = applyTree(f_tree, x_tree, 10_000_000)
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
        const Pi = ctx.trusted.get("Pi")
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
            const pi = applyTree(applyTree(Pi, A_tree, 10_000_000), codFn, 10_000_000)
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
      const Nat = ctx.trusted.get("Nat")
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

// ──────────────────────────── 6. Driver ─────────────────────────────────

export type Decl =
  | { kind: "Def"; name: string; tree: Tree; type?: Tree | null }
  | { kind: "Test"; lhs: Tree; rhs: Tree }

export type ParseItemStats = {
  kind: "let" | "test" | "open" | "field" | "trust"
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
  const trusted: Map<string, Tree> = new Map()
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
    // Church-encode: \sel. sel v1 v2 ... vn
    const n = fieldTrees.length
    if (n === 0) return { tree: LEAF, fields: [] }
    // Build as Cir, then compile
    const selName = "__use_sel"
    let body: Cir = { tag: "var", name: selName }
    for (const ft of fieldTrees) body = cap(body, { tag: "lit", t: ft })
    const cir: Cir = { tag: "lam", x: selName, body }
    const tree = cirToTree(eliminateLams(cir))
    return { tree, fields: fieldNames, fieldTrees, fieldTypes }
  }

  function recordItem(kind: "let" | "test" | "open" | "field" | "trust", name?: string, testIndex?: number): void {
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
      kernel: makeKernelHelpers(trusted),
      trusted,
      scopeDepth: stack.length,
      debugTypeCheck: options.debugTypeCheck ?? false,
    }
  }

  function compileBinding(name: string, type: Expr | null | undefined, body: Expr): { tree: Tree; type?: Tree | null; fields?: string[]; fieldTrees?: Tree[] } {
    let tree: Tree, inferredType: Tree | null = null
    const ctx = getElabCtx()

    if (type != null && type.tag !== "recType" && ctx.kernel) {
      // Typed binding with kernel available: compile annotation as a type.
      // checkAsType handles binders (→ Pi construction) and infers universe levels.
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
      tree = compileExpr(body, lookupEntry, resolveUse)
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

  function runItem(it: Item, target: Decl[], isExport: boolean): void {
    switch (it.tag) {
      case "field": {
        const result = compileBinding(it.name, it.type, it.value)
        target.push({ kind: "Def", name: it.name, tree: result.tree, type: result.type })
        // Register the canonical tree_eq tree id with the runtime fast-path on first definition.
        if (it.name === "tree_eq" && getTreeEqId() === -1) setTreeEqId(result.tree.id)
        registerNativeDispatcherAnchor(it.name, result.tree)
        recordItem("field", it.name)
        return
      }
      case "let": {
        const result = compileBinding(it.name, it.type, it.body)
        if (isExport) {
          // Legacy mode: top-level let exports (for files not yet migrated)
          target.push({ kind: "Def", name: it.name, tree: result.tree, type: result.type })
        }
        if (it.name === "tree_eq" && getTreeEqId() === -1) setTreeEqId(result.tree.id)
        recordItem("let", it.name)
        return
      }
      case "trust": {
        const result = compileBinding(it.name, it.type, it.body)
        trusted.set(it.name, result.tree)
        if (isExport) {
          target.push({ kind: "Def", name: it.name, tree: result.tree, type: result.type })
        }
        recordItem("trust", it.name)
        return
      }
      case "test": {
        compiledTestIndex++
        target.push({
          kind: "Test",
          lhs: compileExpr(it.lhs, lookupEntry, resolveUse),
          rhs: compileExpr(it.rhs, lookupEntry, resolveUse),
        })
        recordItem("test", undefined, compiledTestIndex)
        return
      }
      case "open": {
        const record = resolveExprRecord(it.expr, lookupEntry, resolveUse)
        if (!record || record.fields.length === 0)
          throw new Error("open: expression has no known record fields")
        const tree = record.fieldTrees ? undefined : compileExpr(it.expr, lookupEntry, resolveUse)
        const n = record.fields.length
        for (let i = 0; i < n; i++) {
          const fieldTree = record.fieldTrees ? record.fieldTrees[i] : applyTree(tree!, selectorTree(n, i), 10_000_000)
          const name = record.fields[i]
          const existing = stack[stack.length - 1].get(name)
          if (existing) {
            if (existing.tree?.id === fieldTree.id) continue
            throw new Error(`open: name '${name}' already in scope with different value`)
          }
          const fieldType = record.fieldTypes?.[i] ?? null
          define(name, { tree: fieldTree, type: fieldType })
          if (it.trust) {
            trusted.set(name, fieldTree)
          }
          if (isExport) {
            // Legacy mode: open re-exports opened names
            target.push({ kind: "Def", name, tree: fieldTree })
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
