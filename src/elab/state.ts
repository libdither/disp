// Shared elaboration context: the evaluator session every tree operation goes
// through, the apply budget, the cross-file caches, and the scope-entry types
// shared by the whole src/elab/ pipeline.

import { type Tree, defaultSession } from "../eval/eager.js"
import type { Session, Budget } from "../eval/types.js"
import type { Expr } from "../parse.js"

// The evaluator session the elaborator runs on. Set per-call by parseProgram
// (defaults to the eager defaultSession so helpers called outside a parse — e.g.
// stringToTree from tests — still work). Every tree operation goes through this,
// so an alternate backend (the Phase-3 naive backend, a WASM runtime) can drive
// elaboration. Handles stay typed `Tree` for now but are treated opaquely: built
// via cs.leaf/stem/fork, reduced via cs.apply, compared via cs.equal, inspected
// via cs.classify — never by direct field/.id access. (Mutable module state,
// boxed so every src/elab/ module shares one binding: `elab.cs`.)
export const elab: { cs: Session<Tree> } = { cs: defaultSession }

// Step budget passed to applyTree from the compiler/elaborator. Large
// enough that elaboration of any well-formed program terminates; small
// enough that runaway evaluation aborts before exhausting host memory.
// (Raised 10M -> 40M when Pi/Record/Sigma unified onto the generic negative-
// telescope recognizer (the `tele_walk` walker): the kernel self-check at load now costs
// ~16M steps vs ~8M for the old specialized recognizers — a more general
// recognizer, ~2x heavier per Pi-check; a validated lean fast path could
// reclaim it later, cf. the tree_eq native-fast-path discipline.)
// 400M (raised from 40M, 2026-07-08): the budget is a DIVERGENCE bound, not a
// semantic gate (eval/types.ts § Budget) — it must admit every total program.
// The heaviest single elaborator call today is use_raw.test's kernel-fragment
// `verify` compiled COLD on the eager backend (per-file scoped reclamation
// prunes the freed-node memo entries, so late files recompute): >40M eager
// steps warm-less, well under 400M. The rust backends floor their per-call
// budget at 4G and never bind here; a genuinely diverging call on eager burns
// ~1-2min before erroring, which the S-rule K-discard parity fix (2026-07-07)
// made an abnormal event rather than a kernel-load certainty.
export const APPLY_BUDGET = 400_000_000
export const B = (): Budget => ({ remaining: APPLY_BUDGET, limit: APPLY_BUDGET })

// Auto-verification cache (§ module checking): each module's typed exports are
// checked through the kernel once per process (file content is immutable, and a
// module's verdict is a pure function of its content + the fixed kernel). Keyed by
// absolute path. Without this, every file that opens the kernel would re-run the
// whole kernel self-check at compile time.
export const verifiedModules = new Set<string>()

// Per-session module-record cache: memoize resolveUse(abs, raw) → its export record,
// so a module `use`d by many files (above all the kernel) is elaborated ONCE per
// session instead of re-traversed each time. Keyed by SESSION because the record
// holds Tree handles (arena indices) valid only within one session — a WeakMap so an
// independent per-file session's cache is GC'd with it, while a shared session
// accumulates the whole suite's modules (bounded, a few dozen). Within a session it
// is keyed by abs path (raw imports get a distinct key — `use raw` drops annotations
// and can yield a different tree than `use`).
export const moduleCacheBySession = new WeakMap<Session<Tree>, Map<string, ScopeEntry>>()

// The declaration protocol's VOCABULARY per session: `default_guard`, `let`, `test`,
// `given`. Each is an ordinary library value that a kernel defines, and the driver
// fast-paths a declaration only while the name still means its kernel's value — a
// scope that SHADOWS one opts into the slow path, where the shadowing decorator
// actually runs. (For `test`: an equation lhs strips the marker while `test` is
// unbound or pristine, which also restores the real application head so head-keyed
// elaboration like named-argument calls sees through it.)
//
// A session can hold MORE THAN ONE kernel — the promoted kernel and the archived live one
// both ship a full protocol, and a suite that loads both shares one session. So this
// is a SET per name, not a single winner: every kernel's definition is pristine, and
// the second one to load is not a shadow of the first. Only definitions made by a
// MODULE register; a root file defining one of these names is exactly the override
// the check exists to detect, so it stays a shadow.
const vocabBySession = new WeakMap<Session<Tree>, Map<string, Map<number, Tree>>>()
export const VOCAB_NAMES = new Set(["default_guard", "let", "test", "given"])
export function registerVocab(s: Session<Tree>, name: string, t: Tree): void {
  let m = vocabBySession.get(s)
  if (!m) { m = new Map(); vocabBySession.set(s, m) }
  let ids = m.get(name)
  if (!ids) { ids = new Map(); m.set(name, ids) }
  ids.set(internTreeId(s, t), t)
}
/// Is this in-scope binding one of the kernels' own definitions of `name`?
export function isPristineVocab(s: Session<Tree>, name: string, t: Tree): boolean {
  return vocabBySession.get(s)?.get(name)?.has(internTreeId(s, t)) ?? false
}
/// The registered kernel definitions per vocabulary name (for snapshotting).
export function vocabRoots(s: Session<Tree>): Array<[string, Tree[]]> {
  const m = vocabBySession.get(s)
  return m ? [...m].map(([name, ids]) => [name, [...ids.values()]] as [string, Tree[]]) : []
}

// Session-scoped tree intern ids: a stable small key per distinct handle, used to
// key module instantiations by their fills (MODULES.md § Hermetic scoping).
// Hash-consing makes handle identity coincide with tree identity within a session,
// for object handles (eager TS) and numeric handles (rust backends) alike.
const treeIntern = new WeakMap<Session<Tree>, { map: Map<unknown, number>; next: number }>()
export function internTreeId(s: Session<Tree>, t: Tree): number {
  let st = treeIntern.get(s)
  if (!st) { st = { map: new Map(), next: 1 }; treeIntern.set(s, st) }
  let id = st.map.get(t as unknown)
  if (id == null) { id = st.next++; st.map.set(t as unknown, id) }
  return id
}

// Verified instantiations of FILLED modules, per session. Fill intern ids are
// session-scoped, so a process-global set (verifiedModules, for fill-free files)
// would collide across sessions.
export const verifiedFilledBySession = new WeakMap<Session<Tree>, Set<string>>()

// Scope entry: a compiled tree plus optional MODULE export metadata (set by
// resolveUse, propagated through `let m = use "f"`). Needed because a module's
// fallback value is a Church-encoded record the runtime cut can't read; for
// everything else, projection is the §2.6 cut and needs no metadata.
export interface ScopeEntry {
  tree?: Tree
  type?: Tree | null    // null = untyped, undefined = not yet set
  fields?: string[]
  fieldTrees?: Tree[]
  fieldTypes?: (Tree | null)[]  // per-field types for open
  fieldGuards?: (Tree | null)[] // per-field guards for open (owned names travel with their owner)
  fieldCerts?: (LicenseCert | null)[] // per-field license stamps for open (travel like guards)
  params?: SigParam[]   // named-argument signature (leading binder params + defaults)
  guard?: Tree          // the name's guard (its rebind policy); undefined = default-governed
  cert?: LicenseCert    // driver-stamped license: this binding replaced `old` via a
                        // guard-approved `payload` request. Written ONLY by the driver on a
                        // successful guard consult (never read from module-declared data), so
                        // the open splice can reconcile a licensed export with the original
                        // it replaced without re-trusting the exporting module.
}

// The driver's memo of a successful licensed rebind: `old` is the tree the guard
// let the binding replace, `payload` the request value it approved ({ new, proof }
// under license_guard). Both are session trees (rooted below).
export type LicenseCert = { old: Tree; payload: Tree }

// A binding's named-argument signature: the leading run of its value-lambda's
// (and/or type's) parameters, in declared order, each with its optional default
// recipe. Drives reorderable / default / partial named calls — see sugar.ts
// § named-arg resolution. `name` is the canonical position key; `default`
// (Expr | null) is the omitted-arg fallback.
export type SigParam = { name: string; default: Expr | null }

// Optional callbacks threaded through Expr compilation. `recordTest` lets
// inline `{ ... test lhs = rhs ... }` blocks emit Test decls into the
// driver's `decls` array (Q2). `recordOpen` mirrors parseProgram's
// per-item stats reporting so inline-block items show up in --stats-detail.
export interface CompileSinks {
  recordTest?: (lhs: Tree, rhs: Tree) => void
  recordOpen?: () => void
  recordAnn?: (name: string, ty: Tree, val: Tree) => void
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
    if (e.guard != null) roots.push(e.guard)
    if (e.fieldTrees) for (const t of e.fieldTrees) if (t != null) roots.push(t)
    if (e.fieldTypes) for (const t of e.fieldTypes) if (t != null) roots.push(t)
    if (e.fieldGuards) for (const t of e.fieldGuards) if (t != null) roots.push(t)
    if (e.cert) { roots.push(e.cert.old); roots.push(e.cert.payload) }
    if (e.fieldCerts) for (const c of e.fieldCerts) if (c != null) { roots.push(c.old); roots.push(c.payload) }
  }
  return roots
}
