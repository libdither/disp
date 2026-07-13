// The in-worker disp engine: wraps the real elaborator (src/compile.ts) around
// a rust-eager WASM session, mirroring src/run.ts's runFile semantics, plus
// REPL niceties (value pretty-printing, nat/string decoding, streamed
// per-item progress).

import { parseProgram, type Decl, type ParseItemStats } from '../../../../src/compile.ts'
import { parseItems, type RecMember, type Expr } from '../../../../src/parse.ts'
import { moduleCacheBySession, verifiedModules, verifiedFilledBySession } from '../../../../src/elab/state.ts'
import type { Session } from '../../../../src/eval/types.ts'
import type { Tree } from '../../../../src/eval/eager.ts'
import { RustEagerBrowserSession } from './rust-eager-browser.ts'
import { restoreSnapshot, vfsHash, type RestoreStats } from './snapshot.ts'
import { vfs } from './shims/fs.ts'
import type { ItemEvent, RunOutcome, ValueNode, RawTree } from './protocol.ts'

const PLAYGROUND_PATH = '/lib/tests/playground.disp'
const PRETTY_DEPTH = 14
const PRETTY_LEN = 600
// node budgets for structured values: enough to fill a line or two collapsed;
// unfolding requests a fresh budget from the clicked subtree
const VALUE_BUDGET = 100
const FAIL_BUDGET = 60

// Best-effort line attribution for elaboration errors that carry no `line N`
// (parse errors do; unresolved-variable and per-name errors name the culprit
// instead): pull the identifier out of the message and point at its first
// code occurrence in the buffer. Heuristic by design — the compiler has no
// source-span diagnostics yet.
function guessErrorLine(error: string, source: string): number | undefined {
  const name = error.match(/free variable (\S+)/)?.[1] ?? error.match(/'([^']+)'/)?.[1]
  if (!name || !/^[\w.]+$/.test(name)) return undefined
  const re = new RegExp(`(?:^|[^\\w])${name.replace(/\./g, '\\.')}(?:[^\\w]|$)`)
  const lines = source.split('\n')
  for (let i = 0; i < lines.length; i++) {
    const code = lines[i].split('//')[0]
    if (re.test(code)) return i + 1
  }
  return undefined
}

export class DispRunner {
  #session: RustEagerBrowserSession
  #wasmBytes: ArrayBuffer
  // handle -> name, accumulated across runs (first name wins, like run.ts)
  #names = new Map<number, string>()
  // name -> handle, rebuilt per run (root defs first, then scope exports) —
  // the reverse lookup behind by-name raw-tree requests
  #byName = new Map<string, number>()
  // name -> handle for the most recent run's top-level defs
  #lastDefs = new Map<string, number>()

  constructor(wasmBytes: ArrayBuffer) {
    this.#wasmBytes = wasmBytes
    this.#session = new RustEagerBrowserSession(wasmBytes)
  }

  reset(): void {
    this.#session.dispose()
    this.#session = new RustEagerBrowserSession(this.#wasmBytes)
    this.#names = new Map()
  }

  // Install the shipped precompiled kernel: pre-populate the elaborator's
  // module cache so `open use ".../kernel/prelude.disp"` is a cache hit
  // instead of the ~1-minute self-verification. Refuses (throws) when the
  // snapshot's lib hash differs from this bundle's vfs — stale snapshots
  // never load silently.
  async restoreSnapshot(bytes: Uint8Array): Promise<RestoreStats> {
    const expect = await vfsHash(vfs)
    return restoreSnapshot(this.#session, bytes, expect)
  }

  memoryBytes(): number {
    return this.#session.memoryBytes()
  }

  // the bundled library, for the playground's file tabs
  listFiles(): string[] {
    return [...vfs.keys()].sort()
  }
  readFile(path: string): string | null {
    return vfs.get(path.startsWith('/') ? path : '/' + path) ?? null
  }

  // Mutate the vfs: a written file is immediately `use`-able and an edited
  // one re-elaborates on next use — every cached elaboration keyed by the
  // path is dropped (cache keys are `abs` or `abs\0…`). Session-local: the
  // bundled originals return with a fresh worker.
  writeFile(path: string, text: string): void {
    const p = path.startsWith('/') ? path : '/' + path
    vfs.set(p, text)
    this.#invalidatePath(p)
  }
  removeFile(path: string): void {
    const p = path.startsWith('/') ? path : '/' + path
    if (vfs.delete(p)) this.#invalidatePath(p)
  }
  #invalidatePath(abs: string): void {
    const stale = (k: string) => k === abs || k.startsWith(abs + '\0')
    const cache = moduleCacheBySession.get(this.#session as unknown as Session<Tree>)
    if (cache) for (const k of [...cache.keys()]) if (stale(k)) cache.delete(k)
    for (const k of [...verifiedModules]) if (stale(k)) verifiedModules.delete(k)
    const vf = verifiedFilledBySession.get(this.#session as unknown as Session<Tree>)
    if (vf) for (const k of [...vf]) if (stale(k)) vf.delete(k)
  }

  // Jump-to-definition, purely syntactic: parse the origin buffer, then BFS
  // its `use` graph (each file's vfs text) looking for a top-level definition
  // of `name`. Opens process in REVERSE order per file (a later open rebinds
  // an earlier one's names, so the later one is the likelier target); private
  // `let`s count only in the origin (they never export). A field whose value
  // is a sum-type literal also defines its variants' constructors (the
  // elaborator's auto-declaration). Approximate by design — no given-fills,
  // no licensed-rebind modeling, binder params have no site — but exact
  // enough for navigation, and it needs no elaboration at all.
  findDef(name: string, fromPath: string, fromText: string): { path: string; line: number } | null {
    const norm = (p: string) => (p.startsWith('/') ? p : '/' + p)
    const resolve = (rel: string, from: string): string => {
      if (rel.startsWith('/')) return rel
      const parts = from.split('/').slice(0, -1)
      for (const seg of rel.split('/')) {
        if (seg === '..') parts.pop()
        else if (seg !== '.') parts.push(seg)
      }
      return parts.join('/')
    }
    const defLine = (members: RecMember[], includePrivate: boolean): number | null => {
      for (const it of members) {
        if (it.tag !== 'field') continue
        const head = it.head
        const headName = head?.tag === 'var' ? head.name : null
        if (headName === 'given') continue
        if (!includePrivate && headName === 'let') continue
        if (it.name === name) return it.line ?? 1
        if (it.value) {
          let b: Expr = it.value
          while (b.tag === 'binder') b = b.body
          if (b.tag === 'sumType' && b.variants.some((v) => v.name === name)) return it.line ?? 1
        }
      }
      return null
    }
    const start = norm(fromPath)
    const seen = new Set([start])
    const queue: { path: string; text: string; origin: boolean }[] = [
      { path: start, text: fromText, origin: true }
    ]
    while (queue.length > 0) {
      const { path, text, origin } = queue.shift()!
      let members: RecMember[]
      try {
        members = parseItems(text)
      } catch {
        continue // a mid-edit buffer that doesn't parse can still be BFS'd past via regex below
      } finally {
        // opens by regex (comment hits just add harmless branches), reversed
        const opens: string[] = []
        const re = /\buse\s+(?:raw\s+)?"([^"]+\.disp)"/g
        let m: RegExpExecArray | null
        while ((m = re.exec(text))) opens.push(resolve(m[1], path))
        for (const p of opens.reverse()) {
          const q = norm(p)
          if (seen.has(q)) continue
          seen.add(q)
          const t = vfs.get(q)
          if (t != null) queue.push({ path: q, text: t, origin: false })
        }
      }
      const line = defLine(members, origin)
      if (line != null) return { path, line }
    }
    return null
  }

  run(
    source: string,
    path: string | undefined,
    wantDefPretty: boolean,
    onItem: (e: ItemEvent) => void
  ): RunOutcome {
    const session = this.#session as unknown as Session<Tree>
    const t0 = performance.now()
    const steps0 = this.#session.stats().steps
    const outcome: RunOutcome = {
      ok: true,
      defs: [],
      tests: [],
      elapsedMs: 0,
      steps: 0,
      memBytes: 0
    }
    // Top-level bindings arrive on the item stream — exported fields AND
    // private `let`s (which never become Decls) — keyed by name so a rebind
    // keeps one entry. Captured incrementally, so a mid-file error still
    // reports every binding elaborated before it.
    const rootDefs = new Map<string, { handle: number; line?: number; endLine?: number }>()
    let decls: Decl[] = []
    try {
      decls = parseProgram(source, path ?? PLAYGROUND_PATH, {
        session,
        onItem: (item: ParseItemStats) => {
          if (item.depth === 0 && (item.kind === 'field' || item.kind === 'let') && item.name && item.tree != null)
            rootDefs.set(item.name, { handle: item.tree as unknown as number, line: item.line, endLine: item.endLine })
          onItem({
            type: 'item',
            kind: item.kind,
            name: item.name,
            testIndex: item.testIndex,
            sourcePath: item.sourcePath,
            depth: item.depth,
            line: item.line,
            steps: item.stats.steps
          })
        }
      })
    } catch (e) {
      outcome.ok = false
      outcome.error = e instanceof Error ? e.message : String(e)
      const m = outcome.error.match(/line (\d+)/)
      if (m) outcome.errorLine = Number(m[1])
      else outcome.errorLine = guessErrorLine(outcome.error, source)
    }
    // Names before pretty (the printer is name-aware); `__`-prefixed
    // scaffolding (the REPL's __it) stays out of the name table. Root defs
    // register first (they win over module exports for shared handles),
    // then everything the buffer's opens brought into scope. The reverse
    // (name -> handle) table rebuilds per run — lookups see THIS run's scope.
    this.#byName = new Map()
    for (const [name, d] of rootDefs) {
      if (name.startsWith('__')) continue
      if (!this.#names.has(d.handle)) this.#names.set(d.handle, name)
      if (!this.#byName.has(name)) this.#byName.set(name, d.handle)
    }
    this.#registerScopeNames()
    this.#lastDefs = new Map([...rootDefs].map(([n, d]) => [n, d.handle]))
    for (const [name, d] of rootDefs)
      outcome.defs.push({
        name,
        line: d.line,
        endLine: d.endLine,
        pretty: wantDefPretty ? this.pretty(d.handle, name) : undefined,
        value: wantDefPretty ? this.renderValue(d.handle, VALUE_BUDGET, false, name) : undefined
      })
    let testIdx = 0
    for (const d of decls) {
      if (d.kind !== 'Test') continue
      testIdx++
      const lhs = d.lhs as unknown as number
      const rhs = d.rhs as unknown as number
      const pass = this.#session.equal(lhs, rhs)
      outcome.tests.push({
        index: testIdx,
        line: d.line,
        endLine: d.endLine,
        pass,
        lhs: pass ? undefined : this.pretty(lhs),
        rhs: pass ? undefined : this.pretty(rhs),
        lhsNode: pass ? undefined : this.renderValue(lhs, FAIL_BUDGET),
        rhsNode: pass ? undefined : this.renderValue(rhs, FAIL_BUDGET)
      })
    }
    if (!outcome.error) outcome.ok = outcome.tests.every((t) => t.pass)
    outcome.elapsedMs = performance.now() - t0
    outcome.steps = this.#session.stats().steps - steps0
    // Drop re-derivable caches (apply memo, susp WHNF memo) so the arena's
    // Rust heap stays reusable across runs — the wasm32 4GiB ceiling is real
    // and a verified kernel load already sits near it.
    this.#session.clearCaches()
    outcome.memBytes = this.#session.memoryBytes()
    return outcome
  }

  evalExpr(
    context: string,
    expr: string,
    onItem: (e: ItemEvent) => void,
    path?: string,
    wantDefPretty = false
  ): RunOutcome {
    const source = `${context.trimEnd()}\n__it := (${expr.trim()})\n`
    const outcome = this.run(source, path, wantDefPretty, onItem)
    const h = this.#lastDefs.get('__it')
    if (h !== undefined) {
      outcome.value = this.pretty(h, '__it')
      outcome.valueNode = this.renderValue(h, VALUE_BUDGET, false, '__it')
      outcome.valueHint = this.hint(h)
    }
    outcome.defs = outcome.defs.filter((d) => d.name !== '__it')
    return outcome
  }

  // Register every open'd module export in the handle→name table, so values
  // render with the names actually in scope: `quadruple`'s body shows the
  // NAME `double`, not its expansion — hash-consing makes a scope binding's
  // tree and its embedded occurrences the same handle, and the module cache
  // holds the exports whether the kernel was elaborated cold or restored
  // from the snapshot. Tiny trees are skipped: the smallest constants
  // collide across meanings (§2.6: one tree is true, zero, nil, K's head at
  // once), so naming them would mislabel ordinary structural glue.
  #registerScopeNames(): void {
    const cache = moduleCacheBySession.get(this.#session as unknown as Session<Tree>)
    if (!cache) return
    for (const entry of cache.values()) {
      if (!entry.fields || !entry.fieldTrees) continue
      for (let i = 0; i < entry.fields.length; i++) {
        const name = entry.fields[i]
        const h = entry.fieldTrees[i] as unknown as number
        if (h == null || name.startsWith('__')) continue
        if (!this.#byName.has(name)) this.#byName.set(name, h)
        if (this.#names.has(h)) continue
        if (this.#hasAtLeastNodes(h, 4)) this.#names.set(h, name)
      }
    }
  }

  // early-exit node count: does the tree have at least `min` nodes?
  #hasAtLeastNodes(h: number, min: number): boolean {
    let count = 0
    const stack = [h]
    while (stack.length > 0) {
      const cur = stack.pop()!
      count++
      if (count >= min) return true
      const c = this.#session.classify(cur)
      if (c.tag === 'stem') stack.push(c.child)
      else if (c.tag === 'fork') stack.push(c.left, c.right)
    }
    return false
  }

  // ---- value decoding ----------------------------------------------------

  // Nat: zero = leaf, succ(m) = fork(leaf, m)
  decodeNat(h: number): number | null {
    let n = 0
    let cur = h
    for (;;) {
      const c = this.#session.classify(cur)
      if (c.tag === 'leaf') return n
      if (c.tag !== 'fork') return null
      const l = this.#session.classify(c.left)
      if (l.tag !== 'leaf') return null
      n++
      cur = c.right
      if (n > 1_000_000) return null
    }
  }

  // String: cons-chain fork(natTree(codepoint), rest), nil = leaf
  decodeString(h: number): string | null {
    const codes: number[] = []
    let cur = h
    for (;;) {
      const c = this.#session.classify(cur)
      if (c.tag === 'leaf') break
      if (c.tag !== 'fork') return null
      const code = this.decodeNat(c.left)
      if (code === null || code < 32 || code > 0x10ffff) return null
      codes.push(code)
      cur = c.right
      if (codes.length > 4096) return null
    }
    if (codes.length === 0) return null
    return String.fromCodePoint(...codes)
  }

  // A short human hint for a value: its bound name, a Nat, or a String.
  hint(h: number): string | undefined {
    const name = this.#names.get(h)
    if (name) return name
    const n = this.decodeNat(h)
    if (n !== null && n > 0) return String(n)
    const s = this.decodeString(h)
    if (s !== null) return JSON.stringify(s)
    return undefined
  }

  // Structured, budget-capped rendering: same display priority as the string
  // printer (leaf → nat → string → bound name → raw structure), but each
  // decoded atom and each budget cut keeps its HANDLE, so the UI can ask for
  // that subtree again raw (`rawRoot` — skip decoding at the root only; the
  // click-to-unfold of a recognized atom) or deeper (a fresh budget).
  // `skipName` suppresses the self-alias at a def's own root.
  renderValue(h: number, budget = VALUE_BUDGET, rawRoot = false, skipName?: string): ValueNode {
    const counter = { left: budget }
    return this.#valueNode(h, counter, rawRoot, skipName)
  }

  #valueNode(h: number, counter: { left: number }, raw = false, skipName?: string): ValueNode {
    const cls = this.#session.classify(h)
    if (cls.tag === 'leaf') return { k: 'leaf' }
    if (!raw) {
      const n = this.decodeNat(h)
      if (n !== null && n > 0) return { k: 'nat', n, h }
      const s = this.decodeString(h)
      if (s !== null) return { k: 'str', s, h }
      const name = this.#names.get(h)
      if (name && name !== skipName) return { k: 'name', name, h }
    }
    if (counter.left <= 0) return { k: 'more', h }
    counter.left--
    if (cls.tag === 'stem') return { k: 'stem', c: [this.#valueNode(cls.child, counter)], h }
    return { k: 'fork', c: [this.#valueNode(cls.left, counter), this.#valueNode(cls.right, counter)], h }
  }

  // The COMPLETE structure of a tree — the reduction visualizer's food. By
  // handle or by in-scope name (this run's scope); null when unbound or when
  // the tree exceeds maxNodes (an over-budget def stays symbolic fruit rather
  // than drowning the drawing).
  rawTree(spec: { handle?: number; name?: string }, maxNodes: number): RawTree | null {
    const h = spec.handle ?? (spec.name != null ? this.#byName.get(spec.name) : undefined)
    if (h == null) return null
    const counter = { left: maxNodes }
    const go = (x: number): RawTree | null => {
      if (--counter.left < 0) return null
      const c = this.#session.classify(x)
      if (c.tag === 'leaf') return 0
      if (c.tag === 'stem') {
        const a = go(c.child)
        return a === null ? null : [a]
      }
      const l = go(c.left)
      if (l === null) return null
      const r = go(c.right)
      return r === null ? null : [l, r]
    }
    return go(h)
  }

  // Engine macro-step: apply an in-scope value (by handle or name) to
  // concrete argument trees on the REAL evaluator, returning the result's
  // structure. The visualizer calls this when its own stepping is stuck on a
  // definition too large to ship as a pod — the reduction the elaborator
  // would perform happens here, and only the (usually small) RESULT crosses
  // back. null when unbound, when the apply blows its budget, or when the
  // result itself exceeds maxNodes.
  applySpine(spec: { handle?: number; name?: string }, args: RawTree[], maxNodes: number): RawTree | null {
    const h = spec.handle ?? (spec.name != null ? this.#byName.get(spec.name) : undefined)
    if (h == null) return null
    try {
      let cur = h
      for (const a of args) cur = this.#session.apply(cur, this.#internRaw(a))
      return this.rawTree({ handle: cur }, maxNodes)
    } catch {
      return null // divergence budget, arena pressure — the panel stays stuck
    }
  }

  // build a session tree from its structure via the construction rules:
  // stem x = leaf · x, fork x y = leaf · x · y
  #internRaw(r: RawTree): number {
    const leaf = this.#session.leaf()
    if (r === 0) return leaf
    if (r.length === 1) return this.#session.apply(leaf, this.#internRaw(r[0]))
    return this.#session.apply(this.#session.apply(leaf, this.#internRaw(r[0])), this.#internRaw(r[1]))
  }

  // Applicative-style pretty printer (t x (t y) …), name-aware, capped.
  pretty(h: number, selfName?: string): string {
    const out = this.#render(h, PRETTY_DEPTH, false, selfName)
    return out.length > PRETTY_LEN ? out.slice(0, PRETTY_LEN) + '…' : out
  }

  #render(h: number, depth: number, atom: boolean, selfName?: string): string {
    // Display priority: the leaf is always 't' (the atom everyone knows);
    // then numeral/string decodings (many REPL names alias small trees —
    // `succ zero` and a compiled `{n} -> refl` are literally the same
    // hash-consed fork, and "1" reads better than a stale binding); then a
    // bound name; then raw structure.
    const cls = this.#session.classify(h)
    if (cls.tag === 'leaf') return 't'
    const n = this.decodeNat(h)
    if (n !== null && n > 0) return String(n)
    const s = this.decodeString(h)
    if (s !== null) return JSON.stringify(s)
    const name = this.#names.get(h)
    if (name && name !== selfName) return name
    if (depth <= 0) return '…'
    const c = cls
    if (c.tag === 'stem') {
      const inner = `t ${this.#render(c.child, depth - 1, true)}`
      return atom ? `(${inner})` : inner
    }
    const inner = `t ${this.#render(c.left, depth - 1, true)} ${this.#render(c.right, depth - 1, true)}`
    return atom ? `(${inner})` : inner
  }
}
