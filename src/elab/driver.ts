// The module driver: parseProgram — scope stack, `use`/`use raw` resolution with
// the per-session module cache, deferred end-of-parse auto-verification of typed
// exports through the kernel, and per-item stats reporting.

import { readFileSync } from "node:fs"
import { dirname, resolve as pathResolve } from "node:path"
import { type Tree, defaultSession } from "../eval/eager.js"
import type { Session, EvalStats } from "../eval/types.js"
import { parseItems, type Expr, type RecMember } from "../parse.js"
import { elab, B, verifiedModules, moduleCacheBySession, type ScopeEntry, type CompileSinks } from "./state.js"
import { type Cir, cap, cirToTree, eliminateLams } from "./cir.js"
import { extractSignature } from "./sugar.js"
import { stringToTree, accTree, recordFieldsFromTree } from "./literals.js"
import { exprToCir, resolveExprRecord, compileExpr, compileType, isUniverseTree } from "./expr.js"

export type Decl =
  | { kind: "Def"; name: string; tree: Tree; type?: Tree | null; guard?: Tree | null }
  | { kind: "Test"; lhs: Tree; rhs: Tree }

// The pristine `default_guard` tree per session, captured at its first definition
// (cut.disp). The declaration fast path applies only while the ambient default is
// pristine; a scope that shadows `default_guard` opts its unguarded names into the
// consulting path. (The fast path mirrors the disp definition's semantics exactly —
// the tree_eq native-fast-path discipline: the in-language definition is normative.)
const pristineDefaultGuard = new WeakMap<Session<Tree>, Tree>()

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
  const prev = elab.cs
  elab.cs = session
  try { return parseProgramBody(src, sourcePath, options) } finally { elab.cs = prev }
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
    const al = elab.cs.applyLazy
    return lazyVerify && al ? al.call(elab.cs, f, x) : elab.cs.apply(f, x, B())
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
    let modCache = moduleCacheBySession.get(elab.cs)
    if (!modCache) { modCache = new Map(); moduleCacheBySession.set(elab.cs, modCache) }
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
      const mkr = lookupEntry("make_record")?.tree, lc = lookupEntry("list_const")?.tree
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
    const fieldGuards: (Tree | null)[] = []
    for (const d of fileDecls) {
      if (d.kind === "Def") {
        fieldNames.push(d.name)
        fieldTrees.push(d.tree)
        fieldTypes.push(d.type ?? null)
        fieldGuards.push(d.guard ?? null)
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
        if (fieldTypes[i]) typEntries.push(elab.cs.fork(stringToTree(fieldNames[i]), fieldTypes[i]!))
      if (typEntries.length > 0) {
        const consList = (xs: Tree[]): Tree => xs.reduceRight<Tree>((acc, h) => elab.cs.fork(h, acc), elab.cs.leaf())
        const recordVal = elab.cs.apply(elab.cs.apply(vFormers.mkRecord, consList(fieldNames.map(stringToTree)), B()),
                                   consList(fieldTrees.map(v => elab.cs.apply(vFormers!.listConst, v, B()))), B())
        const typVal = elab.cs.apply(vFormers.Record, consList(typEntries), B())
        // Build the verdict (lazily by default — vApply); defer the force to the
        // single end-of-parse batch (see pendingVerifications). Keeps the elegant
        // whole-record form; the parallelism/laziness transfers to the final force.
        const verdict = vApply(vApply(vFormers.paramApply, typVal), recordVal)
        const okTT = elab.cs.apply(vFormers.ok, vFormers.tt, B())
        pendingVerifications.push({ abs, verdict, okTT })
        scheduledForVerification.add(abs)
      }
    }
    // Church-encode: \sel. sel v1 v2 ... vn
    const n = fieldTrees.length
    if (n === 0) {
      const empty: ScopeEntry = { tree: elab.cs.leaf(), fields: [] }
      modCache.set(cacheKey, empty)
      return empty
    }
    // Build as Cir, then compile
    const selName = "__use_sel"
    let body: Cir = { tag: "var", name: selName }
    for (const ft of fieldTrees) body = cap(body, { tag: "lit", t: ft })
    const cir: Cir = { tag: "lam", x: selName, body }
    const tree = cirToTree(eliminateLams(cir))
    const record: ScopeEntry = { tree, fields: fieldNames, fieldTrees, fieldTypes, fieldGuards }
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
      stats: elab.cs.stats!(),
    })
  }

  // compileParts: compile a binding's value (and annotation) WITHOUT touching scope.
  // The declaration pipeline (declareBinding) decides what actually gets bound — for
  // guarded names the bound tree may differ from the compiled value (the guard's Bind).
  function compileParts(
    name: string,
    type: Expr | null | undefined,
    body: Expr,
    sinks?: CompileSinks,
    raw = false,
  ): { tree: Tree; type: Tree | null } {
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
    return { tree, type: inferredType }
  }

  // finishDefine: bind a tree in scope with its metadata. `body` is the source
  // expression when the tree came straight from one (fast path / idempotent rebind),
  // else null (a guard-produced tree: record metadata is read off the tree itself and
  // the named-arg signature is dropped).
  function finishDefine(
    name: string,
    tree: Tree,
    typeTree: Tree | null,
    body: Expr | null,
    typeExpr: Expr | null | undefined,
    guard: Tree | undefined,
  ): void {
    // Module metadata propagation (`let m = use "f"`): `open m` and projection
    // on the module's Church-record fallback need the export list. Falls back to
    // reading the field header off the compiled tree, so a binding whose value is
    // a §2.6 record (`E := Enum {…}`, any `make_record` result) is `open`-able and
    // projects at compile time.
    const record = (body ? resolveExprRecord(body, lookupEntry, resolveUse) : undefined) ?? recordFieldsFromTree(tree, lookupEntry)
    // Track the named-argument signature (leading lambda params + defaults) so a
    // later `name { … }` call resolves by field name. Pure metadata — the tree is
    // unchanged. Skipped for raw imports (their bodies bypass annotations anyway).
    const params = body ? extractSignature(body, typeExpr ?? null) : undefined
    define(name, { tree, type: typeTree, fields: record?.fields, fieldTrees: record?.fieldTrees, fieldTypes: record?.fieldTypes, params, guard })
  }

  function compileBinding(
    name: string,
    type: Expr | null | undefined,
    body: Expr,
    sinks?: CompileSinks,
    raw = false,
  ): { tree: Tree; type?: Tree | null } {
    const r = compileParts(name, type, body, sinks, raw)
    finishDefine(name, r.tree, r.type, body, type, lookupEntry(name)?.guard)
    return r
  }

  // declareBinding: the declaration pipeline (COMPILATION.typ § Declarations as
  // requests). Fast path = the pre-guard behavior, taken when there is no head, no
  // installed guard on the name, and the ambient default_guard is pristine (or the
  // kernel isn't loaded yet). Slow path builds the request record, applies the head
  // decorator, consults the incumbent guard, and applies its GuardAction.
  function declareBinding(
    name: string,
    typeE: Expr | null | undefined,
    valueE: Expr | null,
    headE: Expr | undefined,
    isLet: boolean,
    sinks: CompileSinks,
    raw: boolean,
  ): { pushDef: boolean; tree?: Tree; type?: Tree | null; guard?: Tree; viaFast: boolean } {
    const S = elab.cs
    const existing = lookupEntry(name)
    const dg = lookupEntry("default_guard")?.tree
    const pristine = pristineDefaultGuard.get(S)
    const ambientShadowed = dg != null && !(pristine != null && S.equal!(dg, pristine))
    if (raw || (!headE && !existing?.guard && !ambientShadowed)) {
      // Fast path. (The parser guarantees headless declarations carry a value.)
      const r = compileBinding(name, typeE, valueE!, sinks, raw)
      return { pushDef: true, tree: r.tree, type: r.type, guard: existing?.guard, viaFast: true }
    }
    // Slow path: compile the pieces.
    let valueTree: Tree | null = null
    let typeTree: Tree | null = null
    if (valueE != null) {
      const p = compileParts(name, typeE, valueE, sinks, raw)
      valueTree = p.tree
      typeTree = p.type
    } else if (typeE != null) {
      typeTree = compileType(typeE, lookupEntry, resolveUse)
    }
    // Idempotence: a headless, tree-identical rebind changes nothing — no consultation.
    if (!headE && valueTree != null && existing?.tree != null && S.equal!(existing.tree, valueTree)) {
      finishDefine(name, valueTree, typeTree, valueE, typeE ?? null, existing.guard)
      return { pushDef: true, tree: valueTree, type: typeTree, guard: existing.guard, viaFast: false }
    }
    const mkr = lookupEntry("make_record")?.tree, lc = lookupEntry("list_const")?.tree
    const tt = lookupEntry("TT")?.tree, ff = lookupEntry("FF")?.tree
    const g = existing?.guard ?? dg
    if (!mkr || !lc || !tt || !ff || !g)
      throw new Error(`declaration of '${name}': guarded declarations need the kernel prelude in scope`)
    const opt = (x: Tree | null): Tree => (x == null ? S.leaf() : S.stem(x))
    const consList = (xs: Tree[]): Tree => xs.reduceRight<Tree>((acc, h) => S.fork(h, acc), S.leaf())
    const names = consList(["value", "ty", "guard", "private"].map(stringToTree))
    const payload = consList([opt(valueTree), opt(typeTree), S.leaf(), isLet ? tt : ff].map(v => S.apply(lc, v, B())))
    let request = S.apply(S.apply(mkr, names, B()), payload, B())
    if (headE) {
      const headTree = compileExpr(headE, lookupEntry, resolveUse, sinks)
      request = S.apply(headTree, request, B())
    }
    const oldOpt = existing?.tree != null ? S.stem(existing.tree) : S.leaf()
    const answer = S.apply(S.apply(g, oldOpt, B()), request, B())
    const top = S.classify!(answer)
    if (top.tag !== "fork" || !S.equal!(top.left, stringToTree("Ok")))
      throw new Error(`declaration of '${name}': rejected by its guard`)
    const act = S.classify!(top.right)
    if (act.tag !== "fork")
      throw new Error(`declaration of '${name}': guard returned a malformed action`)
    if (S.equal!(act.left, stringToTree("Bind"))) {
      finishDefine(name, act.right, typeTree, null, typeE ?? null, existing?.guard)
      return { pushDef: true, tree: act.right, type: typeTree, guard: existing?.guard, viaFast: false }
    }
    if (S.equal!(act.left, stringToTree("Install"))) {
      define(name, { ...(existing ?? {}), guard: act.right })
      return { pushDef: false, guard: act.right, viaFast: false }
    }
    if (S.equal!(act.left, stringToTree("Both"))) {
      const p = S.classify!(act.right)
      if (p.tag !== "fork")
        throw new Error(`declaration of '${name}': guard returned a malformed Both action`)
      finishDefine(name, p.left, typeTree, null, typeE ?? null, p.right)
      return { pushDef: true, tree: p.left, type: typeTree, guard: p.right, viaFast: false }
    }
    throw new Error(`declaration of '${name}': guard returned an unknown action`)
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
        const r = declareBinding(it.name, it.type, it.value, it.head, false, sinks, raw)
        if (r.pushDef) {
          // Redefinition of an exported field is guard-mediated (a rebind): the final
          // binding wins. An UNGUARDED (fast-path) duplicate is still the old accident
          // error, relocated here from the parser (the parser can't see guards).
          const idx = target.findIndex(d => d.kind === "Def" && d.name === it.name)
          const decl: Decl = { kind: "Def", name: it.name, tree: r.tree!, type: r.type, guard: r.guard ?? null }
          if (idx >= 0) {
            if (r.viaFast) throw new Error(`duplicate exported field '${it.name}'`)
            target[idx] = decl
          } else target.push(decl)
          // Register the canonical tree_eq tree id with the runtime fast-path on first definition.
          if (it.name === "tree_eq") elab.cs.recognizeNative?.("tree_eq", r.tree!)
        }
        if (it.name === "default_guard" && r.tree != null && !pristineDefaultGuard.has(elab.cs))
          pristineDefaultGuard.set(elab.cs, r.tree)
        recordItem("field", it.name)
        return
      }
      case "let": {
        const r = declareBinding(it.name, it.type, it.body, undefined, true, sinks, raw)
        if (isExport && r.pushDef) {
          // Legacy mode: top-level let exports (for files not yet migrated)
          target.push({ kind: "Def", name: it.name, tree: r.tree!, type: r.type })
        }
        if (it.name === "tree_eq" && r.tree != null) elab.cs.recognizeNative?.("tree_eq", r.tree)
        if (it.name === "default_guard" && r.tree != null && !pristineDefaultGuard.has(elab.cs))
          pristineDefaultGuard.set(elab.cs, r.tree)
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
          const fieldTree = record.fieldTrees ? record.fieldTrees[i] : elab.cs.apply(tree!, accTree(record.fields[i]), B())
          const name = record.fields[i]
          const existing = stack[stack.length - 1].get(name)
          if (existing) {
            if (existing.tree && elab.cs.equal!(existing.tree, fieldTree)) continue
            throw new Error(`open: name '${name}' already in scope with different value`)
          }
          // A guarded binding in an OUTER frame may not be silently shadowed by an
          // import: rebinding an owned name goes through its guard, explicitly.
          const outer = lookupEntry(name)
          if (outer?.guard && !(outer.tree != null && elab.cs.equal!(outer.tree, fieldTree)))
            throw new Error(`open: '${name}' is guarded; rebind it explicitly through its guard`)
          const fieldType = record.fieldTypes?.[i] ?? null
          define(name, { tree: fieldTree, type: fieldType, guard: record.fieldGuards?.[i] ?? undefined })
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
    if (!elab.cs.equal!(verdict, okTT))
      throw new Error(`type check failed for module ${abs}: an export does not inhabit its declared type (verify returned non-(Ok TT))`)
    verifiedModules.add(abs)
  }
  return decls
}
