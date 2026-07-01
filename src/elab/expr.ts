// Expr -> Cir -> Tree: scope resolution + value/type compilation. exprToCir is
// the one fold over the surface AST (mirrored case-for-case by the deleted
// in-language lib/elab/compile.disp; recover via git when re-validating);
// compileExpr/compileType are the entry points the driver calls.

import type { Tree } from "../eval/eager.js"
import type { Expr } from "../parse.js"
import { elab, B, type ScopeEntry, type CompileSinks } from "./state.js"
import { type Cir, cap, cirToTree, eliminateLams, containsFree, collectFreeVars } from "./cir.js"
import { tryRewriteSelectLazy, tryNamedCall, binderToPi } from "./sugar.js"
import { stringToTree, accTree, recordFieldsFromTree } from "./literals.js"

// Expr → Cir, with scope lookup and use-resolution.
export function exprToCir(
  e: Expr,
  lookupEntry: (name: string) => ScopeEntry | undefined,
  resolveUse: (path: string, raw?: boolean) => ScopeEntry,
  sinks?: CompileSinks,
): Cir {
  const lookup = (name: string) => lookupEntry(name)?.tree
  switch (e.tag) {
    case "leaf": return { tag: "lit", t: elab.cs.leaf() }
    case "num": {
      const zero = lookup("zero")
      const succ = lookup("succ")
      if (!zero || !succ) throw new Error(`numeric literal ${e.value}: zero and succ must be in scope`)
      let result = zero
      for (let i = 0; i < e.value; i++) {
        result = elab.cs.apply(succ, result, B())
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
      // cell — `proj_cell name ty`, or `derive_cell name recipe` for a derived
      // `name := e` member — consed into `t cell (λname. rest)` and wrapped in
      // `Telescope`. Later fields' types and derived recipes compile under lams
      // binding the PRIOR field names, so `{ a : Nat, b := double a }` scopes
      // naturally. (Pi/Sigma emit the same cells, so surface and manual agree.)
      const TelescopeEntry = lookupEntry("Telescope")
      const projCellEntry = lookupEntry("proj_cell")
      const derivCellEntry = lookupEntry("derive_cell")
      if (!TelescopeEntry?.tree || !projCellEntry?.tree || !derivCellEntry?.tree)
        throw new Error("record type literal '{ name : T }': 'Telescope', 'proj_cell', and 'derive_cell' must be in scope (open the kernel prelude)")
      const leafCir: Cir = { tag: "lit", t: elab.cs.leaf() }
      let teleCir: Cir = leafCir
      for (let i = e.fields.length - 1; i >= 0; i--) {
        const f = e.fields[i]
        const priorNames = new Set(e.fields.slice(0, i).map(p => p.name))
        const shadowed = (n: string): ScopeEntry | undefined =>
          priorNames.has(n) ? {} : lookupEntry(n)
        const nameCir: Cir = { tag: "lit", t: stringToTree(f.name) }
        // derived field `name := e` -> derive_cell name recipe; else proj_cell name ty.
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
      const leafCir: Cir = { tag: "lit", t: elab.cs.leaf() }
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
                : elab.cs.apply(targetTree!, accTree(record.fields[i]), B())
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
      // §2.6 record: {x := a; y := b} → make_record ["x","y"] [list_const a, list_const b]
      // — a `prod` over a string-interned name header, read by name through the
      // cut. (make_record/list_const must be in scope, like `match` needs `prod`.)
      const recordValEntry = lookupEntry("make_record")
      const listConstEntry = lookupEntry("list_const")
      if (!recordValEntry?.tree || !listConstEntry?.tree)
        throw new Error("record literal '{ := }': 'make_record' and 'list_const' must be in scope (open the kernel prelude)")
      const recordVal: Cir = { tag: "lit", t: recordValEntry.tree }
      const listConst: Cir = { tag: "lit", t: listConstEntry.tree }
      // names header: a closed cons-chain of string tags.
      let namesTree: Tree = elab.cs.leaf()
      for (let i = e.fields.length - 1; i >= 0; i--)
        namesTree = elab.cs.fork(stringToTree(e.fields[i].name), namesTree)
      // Sequential field scope (telescope discipline): later fields see earlier
      // ones by name ({ a := 2; b := double a }). The record core references
      // each field as a var; each field wraps the rest as ((λname. rest) value),
      // with PRIOR names shadowed while compiling `value` — so a field's value
      // compiles before its own name binds (`respond := respond` resolves
      // outward, field puns included).
      const consCir = (h: Cir, tl: Cir): Cir => cap(cap({ tag: "lit", t: elab.cs.leaf() }, h), tl)
      let payloadCir: Cir = { tag: "lit", t: elab.cs.leaf() }
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
      const make_record = lookupEntry("make_record")?.tree
      const list_const = lookupEntry("list_const")?.tree
      const Record = lookupEntry("Record")?.tree
      if (!make_record || !list_const || !Record || !entry.fields || !entry.fieldTrees)
        return { tag: "lit", t: entry.tree! }
      const consList = (items: Tree[]): Tree => items.reduceRight<Tree>((acc, h) => elab.cs.fork(h, acc), elab.cs.leaf())
      const constWrap = (v: Tree): Tree => elab.cs.apply(list_const, v, B())
      const mkRecord = (names: string[], vals: Tree[]): Tree =>
        elab.cs.apply(elab.cs.apply(make_record, consList(names.map(stringToTree)), B()), consList(vals.map(constWrap)), B())
      const names = entry.fields, vals = entry.fieldTrees, types = entry.fieldTypes ?? []
      const valuesRecord = mkRecord(names, vals)
      // typ = Record [ pair name type ]  over annotated exports (pair = fork(name,type))
      const typEntries: Tree[] = []
      for (let i = 0; i < names.length; i++)
        if (types[i]) typEntries.push(elab.cs.fork(stringToTree(names[i]), types[i]!))
      const typ = elab.cs.apply(Record, consList(typEntries), B())
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
      const leafCir: Cir = { tag: "lit", t: elab.cs.leaf() }

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
      let namesTree: Tree = elab.cs.leaf()
      for (let i = named.length - 1; i >= 0; i--)
        namesTree = elab.cs.fork(stringToTree(named[i].pat), namesTree)
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
export function resolveExprRecord(
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

export function compileExpr(
  e: Expr,
  lookupEntry: (name: string) => ScopeEntry | undefined,
  resolveUse: (path: string, raw?: boolean) => ScopeEntry,
  sinks?: CompileSinks,
): Tree {
  return cirToTree(eliminateLams(exprToCir(e, lookupEntry, resolveUse, sinks)))
}

// isUniverseTree(t, Type): is the annotation `t` EXACTLY the universe `Type`?
// Uses full tree identity, not the root signature: when `Type` is the strict universe
// (a Telescope-shaped wait-form), its signature is SHARED by every Pi/Telescope type, so
// a root-sig match would wrongly treat any `: <Pi-type>` binding as universe-typed and
// compile its value (e.g. Pi's `{A,B} -> …`) as a type. Exact identity matches only the
// universe itself. (Hash-consing makes this O(1); revisit if universe levels `Type i` land.)
export function isUniverseTree(t: Tree, Type: Tree): boolean {
  return elab.cs.equal!(t, Type)
}

// compileType(e): compile an expression that denotes a TYPE to its tree. A
// type-position binder becomes a `Pi` (via binderToPi); every other shape is an
// ordinary expression whose tree IS the type. Falls back to plain compilation
// when `Pi` isn't in scope (non-kernel files carry no checkable types — a binder
// there is an ordinary value lambda).
export function compileType(
  e: Expr,
  lookupEntry: (name: string) => ScopeEntry | undefined,
  resolveUse: (path: string, raw?: boolean) => ScopeEntry,
): Tree {
  const desugared = lookupEntry("Pi")?.tree ? binderToPi(e) : e
  return compileExpr(desugared, lookupEntry, resolveUse)
}
