// The module driver: parseProgram — scope stack, `use`/`use raw` resolution with
// the per-session module cache, deferred end-of-parse auto-verification of typed
// exports through the kernel, and per-item stats reporting.

import { dirname, resolve as pathResolve } from "node:path"
import { type Tree, defaultSession } from "../eval/eager.js"
import type { Session, EvalStats } from "../eval/types.js"
import { parseItems, type Expr, type RecMember } from "../parse.js"
import { elab, B, verifiedModules, moduleCacheBySession, pristineTest, internTreeId, verifiedFilledBySession, type ScopeEntry, type CompileSinks } from "./state.js"
import { parseFileItems, scanGivens, isGivenHead, type GivenSpec } from "./modscan.js"
import { type Cir, cap, cirToTree, eliminateLams } from "./cir.js"
import { extractSignature } from "./sugar.js"
import { stringToTree, accTree, recordFieldsFromTree } from "./literals.js"
import { exprToCir, resolveExprRecord, compileExpr, compileType, isUniverseTree, equationLhs } from "./expr.js"

export type Decl =
  | { kind: "Def"; name: string; tree: Tree; type?: Tree | null; guard?: Tree | null }
  | { kind: "Test"; lhs: Tree; rhs: Tree }

// The pristine `default_guard` tree per session, captured at its first definition
// (cut.disp). The declaration fast path applies only while the ambient default is
// pristine; a scope that shadows `default_guard` opts its unguarded names into the
// consulting path. (The fast path mirrors the disp definition's semantics exactly —
// the tree_eq native-fast-path discipline: the in-language definition is normative.)
const pristineDefaultGuard = new WeakMap<Session<Tree>, Tree>()
// Likewise the pristine `let` decorator (cut.disp): a `let`-headed declaration takes
// the fast private path while `let` is unbound (bootstrap: files before/without the
// kernel — the host fallback of identical semantics) or pristine; a scope that
// shadows `let` routes its lets through the shadowing decorator.
const pristineLet = new WeakMap<Session<Tree>, Tree>()
// And the pristine `given` decorator (cut.disp): a `given`-headed declaration is a
// MODULE DEPENDENCY (MODULES.md) handled by the driver directly while `given` is
// unbound or pristine; a shadowed `given` routes the slow path, where a request
// arriving with `param := TT` is rejected (dynamic givens are unsupported — fills
// resolve against the syntactic pre-scan).
const pristineGiven = new WeakMap<Session<Tree>, Tree>()

// A `let x := e` declaration is an ordinary decorated declaration whose head is the
// single identifier `let` (the private-write request decorator).
const isLetHead = (head: Expr | undefined): boolean => head?.tag === "var" && head.name === "let"
// Fields that EXPORT: every field except `let`-headed and `given`-headed ones
// (private writes and dependency binders respectively).
const hasExportFields = (items: RecMember[]): boolean =>
  items.some(it => it.tag === "field" && !isLetHead(it.head) && !isGivenHead(it.head))

// Per-file elaboration context: the module dependencies (givens) declared by the
// file's pre-scan, the fills supplied at the use site, and bookkeeping.
type FileCtx = {
  isRoot: boolean
  raw: boolean
  abs?: string
  fills: Map<string, Tree>
  givens: GivenSpec[]
  givenSeen: Set<string>
}
const rootCtx = (): FileCtx =>
  ({ isRoot: true, raw: false, fills: new Map(), givens: [], givenSeen: new Set() })

export type ParseItemStats = {
  kind: "let" | "test" | "open" | "field" | "given"
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
  // Entries are module-export checks (marked verified in markSet on success) and
  // given-fill checks (`param_apply T fill`, the well-typed-linking half — no memo:
  // the instantiation cache already dedups them per fill).
  const pendingVerifications: { label: string; verdict: Tree; okTT: Tree; markKey?: string; markSet?: Set<string> }[] = []
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

  function resolveUse(path: string, raw = false, fills?: Map<string, Tree>): ScopeEntry {
    const abs = pathResolve(dirStack[dirStack.length - 1], path)
    const items = parseFileItems(abs)
    // Module dependencies (MODULES.md): validate the supplied fills against the
    // file's declared givens BEFORE elaborating. Fills are explicit; a missing
    // fill without a default is an error (raw fills missing givens with `t` —
    // kernel givens are annotation-only, and raw drops annotations).
    const givens = scanGivens(items)
    const supplied = fills ?? new Map<string, Tree>()
    for (const k of supplied.keys())
      if (!givens.some(g => g.name === k))
        throw new Error(`use ${path}: unknown fill '${k}' (givens: ${givens.map(g => g.name).join(", ") || "none"})`)
    if (!raw) {
      const missing = givens.filter(g => !supplied.has(g.name) && g.dflt == null).map(g => g.name)
      if (missing.length > 0)
        throw new Error(`use ${path}: unfilled given(s) ${missing.join(", ")} — supply them: use "${path}" { ${missing.map(n => `${n} := …`).join(", ")} }`)
    }
    // Instantiation key: one component per given — explicit fill = session intern
    // id of the fill tree, default = "d" (per-file constant), raw-missing = "t".
    const fillKey = givens.map(g =>
      supplied.has(g.name) ? `#${internTreeId(elab.cs, supplied.get(g.name)!)}` : (g.dflt != null && !raw ? "d" : "t")).join(",")
    // Module cache (per session): return the already-elaborated instantiation if
    // present. Checked BEFORE cycle detection on purpose — a cached module is fully
    // loaded, so returning it can't mask a real cycle (a module still mid-load is
    // not yet cached, so it still trips the loadedFiles guard below).
    let modCache = moduleCacheBySession.get(elab.cs)
    if (!modCache) { modCache = new Map(); moduleCacheBySession.set(elab.cs, modCache) }
    const cacheKey = `${abs}\0${raw ? "raw" : ""}\0${fillKey}`
    const hit = modCache.get(cacheKey)
    if (hit) return hit
    if (loadedFiles.has(abs)) throw new Error(`use: circular dependency on ${abs}`)
    loadedFiles.add(abs)
    dirStack.push(dirname(abs))
    sourceStack.push(abs)
    // Hermetic scoping (MODULES.md): the used file elaborates against a FRESH
    // stack — its own definitions, its own opens, its givens — never the use
    // site's ambient scope. This is what makes the instantiation cache sound
    // (a module is a pure function of its content and fills), keeps policy
    // shadowing (`let`/`test`/`default_guard`) file-local, and turns silent
    // capture into an unbound-variable error naming the file that forgot an
    // import.
    const savedFrames = stack.splice(0, stack.length, new Map())
    const fileDecls: Decl[] = []
    // A file that exports nothing of its own (no non-`let` fields — e.g. the kernel
    // barrel, which is pure `open`s) re-exports what it opens; a field-bearing file
    // exports only its fields. (The old legacy mode where top-level lets exported is
    // gone: `let` marks the write private everywhere.)
    const hasFields = hasExportFields(items)
    const ctx: FileCtx = { isRoot: false, raw, abs, fills: supplied, givens, givenSeen: new Set() }
    // Kernel formers needed to auto-verify this module, captured while its own scope
    // is still on the stack (popped in `finally`). Null if the kernel isn't in scope
    // (a file with no checkable annotations) — verification is then skipped.
    let vFormers: { paramApply: Tree; Record: Tree; mkRecord: Tree; listConst: Tree; ok: Tree; tt: Tree } | null = null
    try {
      for (const it of items) {
        try {
          runItem(it, fileDecls, !hasFields, ctx)
        } catch (err) {
          // Attach the file to elaboration errors (hermetic scoping makes a missing
          // import an unbound-variable error naming the file that forgot it).
          const msg = err instanceof Error ? err.message : String(err)
          if (msg.startsWith("in ")) throw err // already attributed by a nested use
          const name = it.tag === "field" ? ` (at '${it.name}')` : it.tag === "open" ? " (at an open)" : ""
          throw new Error(`in ${abs}${name}: ${msg}`)
        }
      }
      const pa = lookupEntry("param_apply")?.tree, rec = lookupEntry("Record")?.tree
      const mkr = lookupEntry("make_record")?.tree, lc = lookupEntry("list_const")?.tree
      const ok = lookupEntry("Ok")?.tree, tt = lookupEntry("TT")?.tree
      if (pa && rec && mkr && lc && ok && tt) vFormers = { paramApply: pa, Record: rec, mkRecord: mkr, listConst: lc, ok, tt }
    } finally {
      stack.splice(0, stack.length, ...savedFrames)
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
    // A FILLED instantiation verifies per fill; its memo is per session (fill intern
    // ids are session-scoped), while fill-free files keep the process-global memo.
    const filled = givens.length > 0
    const verKey = filled ? `${abs}\0${fillKey}` : abs
    let verSet: Set<string>
    if (filled) {
      let s = verifiedFilledBySession.get(elab.cs)
      if (!s) { s = new Set(); verifiedFilledBySession.set(elab.cs, s) }
      verSet = s
    } else verSet = verifiedModules
    if (!raw && vFormers && !verSet.has(verKey) && !scheduledForVerification.has(verKey)) {
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
        pendingVerifications.push({ label: `module ${abs}`, verdict, okTT, markKey: verKey, markSet: verSet })
        scheduledForVerification.add(verKey)
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

  function recordItem(kind: "let" | "test" | "open" | "field" | "given", name?: string, testIndex?: number): void {
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
  // requests). Fast path = the pre-guard behavior, taken when there is no head (or
  // the head is the pristine/unbound `let` decorator), no installed guard on the
  // name, and the ambient default_guard is pristine (or the kernel isn't loaded
  // yet). Slow path builds the request record, applies the head decorator, consults
  // the incumbent guard, and applies its GuardAction.
  function declareBinding(
    name: string,
    typeE: Expr | null | undefined,
    valueE: Expr | null,
    headE: Expr | undefined,
    sinks: CompileSinks,
    raw: boolean,
  ): { pushDef: boolean; tree?: Tree; type?: Tree | null; guard?: Tree; viaFast: boolean; priv?: boolean } {
    const S = elab.cs
    const existing = lookupEntry(name)
    const letHeaded = isLetHead(headE)
    const dg = lookupEntry("default_guard")?.tree
    const pristine = pristineDefaultGuard.get(S)
    const ambientShadowed = dg != null && !(pristine != null && S.equal!(dg, pristine))
    // A `let` head fast-paths while it means the library decorator verbatim: unbound
    // (bootstrap fallback of identical semantics) or tree-identical to the pristine
    // capture. A shadowed `let` falls through to the slow path, where the head
    // compiles to the shadowing decorator and actually runs.
    const letTree = letHeaded ? lookupEntry("let")?.tree : undefined
    const pLet = pristineLet.get(S)
    const letPristine = letHeaded && (letTree == null || (pLet != null && S.equal!(letTree, pLet)))
    if ((raw || ((!headE || letPristine) && !existing?.guard && !ambientShadowed)) && valueE != null) {
      // Fast path. (The parser guarantees headless declarations carry a value;
      // a valueless decorated declaration always consults, below.)
      const r = compileBinding(name, typeE, valueE, sinks, raw)
      return { pushDef: true, tree: r.tree, type: r.type, guard: existing?.guard, viaFast: true, priv: letHeaded || undefined }
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
    // The base request is public and non-param (`private := FF; param := FF`),
    // exactly `base v` in cut.disp; privacy and param are the head's job (the
    // `let` and `given` decorators set them).
    const names = consList(["value", "ty", "guard", "private", "param"].map(stringToTree))
    const payload = consList([opt(valueTree), opt(typeTree), S.leaf(), ff, ff].map(v => S.apply(lc, v, B())))
    let request = S.apply(S.apply(mkr, names, B()), payload, B())
    if (headE) {
      const headTree = compileExpr(headE, lookupEntry, resolveUse, sinks)
      request = S.apply(headTree, request, B())
    }
    // Privacy is declarer intent, so it is read off the FINAL request (a head like
    // `let` may set it), not off the guard's answer.
    const privTree = S.apply(request, accTree("private"), B())
    const isPrivate = S.equal!(privTree, tt)
    // A param request arriving through a CUSTOM decorator: dynamic givens are
    // unsupported — fills resolve against the syntactic pre-scan (modscan.ts).
    const paramTree = S.apply(request, accTree("param"), B())
    if (S.equal!(paramTree, tt))
      throw new Error(`declaration of '${name}': the decorator produced a param (given) request — dynamic givens are unsupported; declare with a literal 'given' head`)
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
      return { pushDef: true, tree: act.right, type: typeTree, guard: existing?.guard, viaFast: false, priv: isPrivate }
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
      return { pushDef: true, tree: p.left, type: typeTree, guard: p.right, viaFast: false, priv: isPrivate }
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

  // handleGiven: a module dependency declaration (MODULES.md). Not a bind request —
  // a binder introduction the driver interprets directly: bind the fill (explicit
  // beats default; raw fills missing ones with `t`), and schedule the well-typed-
  // linking check `param_apply T fill` into the deferred batch (skipped raw, and
  // gradual when the kernel is not in this module's scope, like annotations).
  function handleGiven(it: Extract<RecMember, { tag: "field" }>, sinks: CompileSinks, ctx: FileCtx): void {
    if (ctx.isRoot)
      throw new Error(`given '${it.name}': the root module cannot declare dependencies (givens are filled at a use site)`)
    if (ctx.givenSeen.has(it.name))
      throw new Error(`given '${it.name}': duplicate dependency`)
    ctx.givenSeen.add(it.name)
    if (stack[stack.length - 1].has(it.name))
      throw new Error(`given '${it.name}': the name is already bound in this module`)
    if (lookupEntry(it.name)?.guard)
      throw new Error(`given '${it.name}': collides with a guarded name`)
    if (it.type == null && !ctx.raw)
      throw new Error(`given '${it.name}': a dependency needs a type annotation`)
    let fill = ctx.fills.get(it.name)
    if (fill == null && it.value != null && !ctx.raw)
      fill = compileExpr(it.value, lookupEntry, resolveUse, sinks) // the default, in module scope
    if (fill == null) {
      if (!ctx.raw) throw new Error(`given '${it.name}': unfilled`) // pre-checked in resolveUse; defensive
      fill = elab.cs.leaf()
    }
    // The fill binds BEFORE the annotation compiles, so a given's type may reference
    // the given itself (the self-typed universe dependency `given Type : Type`
    // checks the fill against itself, which is R6) as well as earlier givens.
    define(it.name, { tree: fill })
    const typeTree = !ctx.raw && it.type != null ? compileType(it.type, lookupEntry, resolveUse) : null
    if (typeTree != null) define(it.name, { tree: fill, type: typeTree })
    if (!ctx.raw && typeTree != null) {
      const pa = lookupEntry("param_apply")?.tree, ok = lookupEntry("Ok")?.tree, tt = lookupEntry("TT")?.tree
      if (pa && ok && tt) {
        pendingVerifications.push({
          label: `given '${it.name}' of ${ctx.abs}`,
          verdict: vApply(vApply(pa, typeTree), fill),
          okTT: elab.cs.apply(ok, tt, B()),
        })
      }
    }
  }

  function runItem(it: RecMember, target: Decl[], isExport: boolean, ctx: FileCtx): void {
    const sinks = makeSinks(target)
    const raw = ctx.raw
    switch (it.tag) {
      case "field": {
        // A `given`-headed declaration is a module dependency, interpreted by the
        // driver while `given` is unbound or pristine; a shadowed `given` falls
        // through to the request machinery, where a param request is rejected.
        if (isGivenHead(it.head)) {
          const g = lookupEntry("given")?.tree
          const pg = pristineGiven.get(elab.cs)
          if (g == null || (pg != null && elab.cs.equal!(g, pg))) {
            handleGiven(it, sinks, ctx)
            recordItem("given", it.name)
            return
          }
        }
        // `let x := e` arrives here as a decorated declaration (head = `let`);
        // the tag "let" member only survives inside braces (the lexical form).
        const r = declareBinding(it.name, it.type, it.value, it.head, sinks, raw)
        if (r.pushDef && !r.priv) {
          // Redefinition of an exported field is guard-mediated (a rebind): the final
          // binding wins. An UNGUARDED (fast-path) duplicate is still the old accident
          // error, relocated here from the parser (the parser can't see guards).
          const idx = target.findIndex(d => d.kind === "Def" && d.name === it.name)
          const decl: Decl = { kind: "Def", name: it.name, tree: r.tree!, type: r.type, guard: r.guard ?? null }
          if (idx >= 0) {
            if (r.viaFast) throw new Error(`duplicate exported field '${it.name}'`)
            target[idx] = decl
          } else target.push(decl)
        }
        // Register the canonical tree_eq tree id with the runtime fast-path on first
        // definition (also when bound privately via `let`).
        if (it.name === "tree_eq" && r.pushDef && r.tree != null) elab.cs.recognizeNative?.("tree_eq", r.tree)
        if (it.name === "default_guard" && r.tree != null && !pristineDefaultGuard.has(elab.cs))
          pristineDefaultGuard.set(elab.cs, r.tree)
        if (it.name === "let" && r.tree != null && !pristineLet.has(elab.cs))
          pristineLet.set(elab.cs, r.tree)
        if (it.name === "test" && r.tree != null && !pristineTest.has(elab.cs))
          pristineTest.set(elab.cs, r.tree)
        if (it.name === "given" && r.tree != null && !pristineGiven.has(elab.cs))
          pristineGiven.set(elab.cs, r.tree)
        recordItem(isLetHead(it.head) ? "let" : "field", it.name)
        return
      }
      case "test": {
        compiledTestIndex++
        target.push({
          kind: "Test",
          lhs: compileExpr(equationLhs(it.lhs, lookupEntry), lookupEntry, resolveUse, sinks),
          rhs: compileExpr(it.rhs, lookupEntry, resolveUse, sinks),
        })
        recordItem("test", undefined, compiledTestIndex)
        return
      }
      case "let": // the lexical (braced) form never reaches item level
        throw new Error(`internal: lexical let member '${it.name}' at item level`)
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
  const hasFields = hasExportFields(items)
  const rc = rootCtx()
  for (const it of items) runItem(it, decls, !hasFields, rc)
  // Force ALL deferred verifications at once (module exports + given fills). On a
  // lazy backend this is where the kernel verification walks actually run — one
  // end-of-parse batch over a shared hash-consed arena, so identical sub-checks
  // across modules share work (and a future parallel backend can fan the independent
  // verdicts out). `equal` forces each verdict to NF and compares to `Ok TT` — a
  // closed force, so the result equals eager's.
  for (const p of pendingVerifications) {
    if (!elab.cs.equal!(p.verdict, p.okTT))
      throw new Error(`type check failed for ${p.label}: the value does not inhabit its declared type (returned non-(Ok TT))`)
    if (p.markKey && p.markSet) p.markSet.add(p.markKey)
  }
  return decls
}
