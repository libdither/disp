// The in-worker disp engine: wraps the real elaborator (src/compile.ts) around
// a rust-eager WASM session, mirroring src/run.ts's runFile semantics, plus
// REPL niceties (value pretty-printing, nat/string decoding, streamed
// per-item progress).

import { parseProgram, type Decl, type ParseItemStats } from '../../../../src/compile.ts'
import { parseItems, parseExpr, type RecMember, type Expr } from '../../../../src/parse.ts'
import { moduleCacheBySession, verifiedModules, verifiedFilledBySession, elab, type ScopeEntry } from '../../../../src/elab/state.ts'
import { exprToCir } from '../../../../src/elab/expr.ts'
import { eliminateLams, cirToTree, type Cir } from '../../../../src/elab/cir.ts'
import type { Session } from '../../../../src/eval/types.ts'
import type { Tree } from '../../../../src/eval/eager.ts'
import { RustEagerBrowserSession } from './rust-eager-browser.ts'
import { restoreSnapshot, vfsHash, type RestoreStats } from './snapshot.ts'
import { vfs } from './shims/fs.ts'
import { bracketReplay } from './bracket.ts'
import { G_LEAF, G_STEM, G_FORK, G_APP, G_VAR, G_CUT, G_LAM, G_HELD_K } from './protocol.ts'
import type { ItemEvent, RunOutcome, ValueNode, RawTree, Graph, TermDump, BracketDump, ScopeItem } from './protocol.ts'

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

const resolveRel = (rel: string, from: string): string => {
  if (rel.startsWith('/')) return rel
  const parts = from.split('/').slice(0, -1)
  for (const seg of rel.split('/')) {
    if (seg === '..') parts.pop()
    else if (seg !== '.') parts.push(seg)
  }
  return parts.join('/')
}

function openPaths(text: string, from: string): string[] {
  const out: string[] = []
  const re = /\bopen\s+use\s+(?:raw\s+)?"([^"]+\.disp)"/g
  let m: RegExpExecArray | null
  while ((m = re.exec(text))) out.push(resolveRel(m[1], from))
  return out.reverse() // a later open shadows an earlier one
}

/// The binder names of a def's value: `{a, b} => …`, and past the `self` of `fix ({self, a} => …)`.
function paramsOf(value: Expr | null | undefined): string[] | undefined {
  if (!value) return undefined
  let drop = 0
  let b = value
  if (b.tag === 'app' && b.f.tag === 'var' && b.f.name === 'fix') {
    b = b.x
    drop = 1
  }
  const out: string[] = []
  while (b.tag === 'binder') {
    for (const p of b.params) out.push(p.name ?? '_')
    b = b.body
  }
  return out.length > drop ? out.slice(drop) : undefined
}

function indexFile(path: string, text: string): ScopeItem[] {
  let members: RecMember[]
  try {
    members = parseItems(text)
  } catch {
    return []
  }
  const lines = text.split('\n')
  const items: ScopeItem[] = []
  for (const it of members) {
    if (it.tag !== 'field' || !it.line) continue
    const headName = it.head?.tag === 'var' ? it.head.name : null
    if (headName === 'let' || headName === 'given') continue
    const src = lines[it.line - 1] ?? ''
    const record = /^record\s+([A-Za-z_][\w']*)/.exec(src)?.[1]
    if (record && it.name !== record) {
      items.push({ name: it.name, path, line: it.line, params: ['r'], doc: `The ${it.name.split('.').pop()} field of a ${record}` })
      continue
    }
    const sig = src.match(/^\s*(?:\S+\s+)*?[\w']+\s*::?(?!=)\s*(.*?)\s*(?::=>?|$)/)?.[1]
    const doc: string[] = []
    for (let k = it.line - 2; k >= 0; k--) {
      const c = (lines[k] ?? '').trim()
      if (!c.startsWith('//') || /^\/\/\s*[-=─]{3,}/.test(c)) break
      doc.unshift(c.replace(/^\/\/\/?\s?/, ''))
      if (doc.length >= 6) break
    }
    items.push({ name: it.name, path, line: it.line, sig: sig || undefined, params: paramsOf(it.value), doc: doc.join('\n') || undefined })
    let b = it.value
    while (b?.tag === 'binder') b = b.body
    if (b?.tag === 'sumType')
      for (const v of b.variants) items.push({ name: v.name, path, line: it.line, doc: `a case of ${it.name}` })
  }
  return items
}

/// `test f x` parses as ((test f) x); the marker is the identity, so drop it from the spine.
function stripTestMarker(e: Expr): Expr {
  if (e.tag !== 'app') return e
  if (e.f.tag === 'var' && e.f.name === 'test') return e.x
  const f = stripTestMarker(e.f)
  return f === e.f ? e : { ...e, f }
}

/// The expression a line's member stands for: a def's value, or one side of a test.
function memberAt(text: string, line: number, side: 'lhs' | 'rhs'): Expr | null {
  for (const it of parseItems(text)) {
    if (!('line' in it) || !it.line) continue
    if (line < it.line || line > (it.endLine ?? it.line)) continue
    if (it.tag === 'field') return it.value
    if (it.tag === 'test') {
      return stripTestMarker(it[side])
    }
  }
  return null
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
    const rootDefs = new Map<string, { handle: number; line?: number; endLine?: number; local?: boolean }>()
    const costs: { line: number; steps: number }[] = []
    let lastSteps = steps0
    let decls: Decl[] = []
    try {
      decls = parseProgram(source, path ?? PLAYGROUND_PATH, {
        session,
        onItem: (item: ParseItemStats) => {
          if (item.depth === 0 && (item.kind === 'field' || item.kind === 'let') && item.name && item.tree != null)
            rootDefs.set(item.name, { handle: item.tree as unknown as number, line: item.line, endLine: item.endLine, local: item.kind === 'let' })
          if (item.depth === 0) {
            if (item.line) costs.push({ line: item.line, steps: item.stats.steps - lastSteps })
            lastSteps = item.stats.steps
          }
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
      const off = outcome.error.match(/ at offset (\d+)/)
      if (m) outcome.errorLine = Number(m[1])
      else if (off) {
        // the tokenizer counts characters; people count lines
        outcome.errorLine = source.slice(0, Number(off[1])).split('\n').length
        outcome.error = outcome.error.replace(off[0], ` on line ${outcome.errorLine}`)
      } else outcome.errorLine = guessErrorLine(outcome.error, source)
      outcome.error = outcome.error.replace(/^cirToTree: unresolved free variable (\S+)/, "`$1` isn't defined here")
    }
    // Names before pretty (the printer is name-aware); `__`-prefixed
    // scaffolding (the REPL's __it) stays out of the name table. Root defs
    // register first (they win over module exports for shared handles),
    // then everything the buffer's opens brought into scope. The reverse
    // (name -> handle) table rebuilds per run — lookups see THIS run's scope.
    this.#byName = new Map()
    for (const [name, d] of rootDefs) {
      if (name.startsWith('__')) continue
      if (!d.local && !this.#names.has(d.handle)) this.#names.set(d.handle, name)
      if (!this.#byName.has(name)) this.#byName.set(name, d.handle)
    }
    this.#registerScopeNames()
    // a `let` alias (`let hyp_type := pair_snd`) names its tree only when nothing else does
    for (const [name, d] of rootDefs)
      if (d.local && !name.startsWith('__') && !this.#names.has(d.handle) && this.#hasAtLeastNodes(d.handle, 4)) this.#names.set(d.handle, name)
    this.#lastDefs = new Map([...rootDefs].map(([n, d]) => [n, d.handle]))
    for (const [name, d] of rootDefs)
      outcome.defs.push({
        name,
        line: d.line,
        endLine: d.endLine,
        h: d.handle,
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
        lhsH: lhs,
        rhsH: rhs,
        lhs: pass ? undefined : this.pretty(lhs),
        rhs: pass ? undefined : this.pretty(rhs),
        lhsNode: pass ? undefined : this.renderValue(lhs, FAIL_BUDGET),
        rhsNode: pass ? undefined : this.renderValue(rhs, FAIL_BUDGET)
      })
    }
    if (!outcome.error) outcome.ok = outcome.tests.every((t) => t.pass)
    outcome.costs = costs
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

  // ---- the inspector's feed ------------------------------------------------

  // Top-level names reachable through `open use`, nearest file first and each
  // name once, for autocomplete and hover. Syntactic like findDef, cached per
  // file, so it needs no elaboration and works on a buffer mid-edit.
  #fileIndex = new Map<string, { text: string; items: ScopeItem[]; opens: string[] }>()
  scopeItems(fromPath: string, fromText: string): ScopeItem[] {
    const start = fromPath.startsWith('/') ? fromPath : '/' + fromPath
    const out: ScopeItem[] = []
    const names = new Set<string>()
    const seen = new Set([start])
    const queue = openPaths(fromText, start)
    while (queue.length > 0) {
      const path = queue.shift()!
      if (seen.has(path)) continue
      seen.add(path)
      const text = vfs.get(path)
      if (text == null) continue
      let idx = this.#fileIndex.get(path)
      if (!idx || idx.text !== text) {
        idx = { text, items: indexFile(path, text), opens: openPaths(text, path) }
        this.#fileIndex.set(path, idx)
      }
      for (const it of idx.items) {
        if (names.has(it.name)) continue
        names.add(it.name)
        out.push(it)
      }
      queue.push(...idx.opens)
    }
    return out
  }

  // Handle -> name in the CURRENT scope: nameless tiny trees stay structural
  // (see #registerScopeNames) and stale names from earlier edits don't leak in.
  #labelOf(h: number): string | undefined {
    const name = this.#names.get(h)
    return name && this.#byName.get(name) === h && this.#hasAtLeastNodes(h, 4) ? name : undefined
  }

  #dumper(maxNodes: number) {
    const tags: number[] = []
    const as: number[] = []
    const bs: number[] = []
    const strs: string[] = []
    const labels: [number, string][] = []
    const byHandle = new Map<number, number>()
    const byShape = new Map<string, number>()
    const emit = (tag: number, a: number, b: number): number => {
      const key = `${tag},${a},${b}`
      const hit = byShape.get(key)
      if (hit !== undefined) return hit
      tags.push(tag)
      as.push(a)
      bs.push(b)
      byShape.set(key, tags.length - 1)
      return tags.length - 1
    }
    const session = this.#session
    const add = (root: number): number => {
      const stack = [root]
      while (stack.length > 0) {
        const h = stack[stack.length - 1]
        if (byHandle.has(h)) {
          stack.pop()
          continue
        }
        let idx: number
        const c = session.classify(h)
        if (c.tag === 'leaf') idx = emit(G_LEAF, 0, 0)
        else if (tags.length >= maxNodes) idx = emit(G_CUT, h, 0)
        else if (c.tag === 'stem') {
          const k = byHandle.get(c.child)
          if (k === undefined) {
            stack.push(c.child)
            continue
          }
          idx = emit(G_STEM, k, 0)
        } else {
          const l = byHandle.get(c.left)
          const r = byHandle.get(c.right)
          if (l === undefined || r === undefined) {
            if (r === undefined) stack.push(c.right)
            if (l === undefined) stack.push(c.left)
            continue
          }
          idx = emit(G_FORK, l, r)
        }
        byHandle.set(h, idx)
        const label = this.#labelOf(h)
        if (label) labels.push([idx, label])
        stack.pop()
      }
      return byHandle.get(root)!
    }
    return {
      add,
      emit,
      str: (s: string) => {
        const i = strs.indexOf(s)
        return i >= 0 ? i : strs.push(s) - 1
      },
      finish: (roots: number[]): Graph => {
        const nodes = new Int32Array(tags.length * 3)
        for (let i = 0; i < tags.length; i++) {
          nodes[i * 3] = tags[i]
          nodes[i * 3 + 1] = as[i]
          nodes[i * 3 + 2] = bs[i]
        }
        return { nodes, strs, labels, roots }
      }
    }
  }

  graph(handles: number[], names: string[], maxNodes: number): Graph {
    const d = this.#dumper(maxNodes)
    const roots = handles.map((h) => d.add(h))
    for (const n of names) {
      const h = this.#byName.get(n)
      roots.push(h === undefined ? -1 : d.add(h))
    }
    return d.finish(roots)
  }

  // Compile an expression exactly as the elaborator does (same desugaring,
  // same bracket abstraction, same compile-time shortcuts as cirToTree) but
  // leave every application it would have evaluated HELD, so the inspector can
  // run it step by step. The real value comes along to check the result against.
  term(spec: { expr?: string; path?: string; text?: string; line?: number; side?: 'lhs' | 'rhs'; value?: number; maxNodes: number }): TermDump {
    const d = this.#dumper(spec.maxNodes)
    const fail = (error: string, source = spec.expr ?? ''): TermDump => ({
      graph: d.finish([]),
      source,
      error
    })
    let expr: Expr | null = null
    try {
      expr = spec.expr != null ? parseExpr(spec.expr) : memberAt(spec.text ?? '', spec.line ?? 0, spec.side ?? 'lhs')
    } catch (e) {
      return fail(e instanceof Error ? e.message : String(e))
    }
    if (!expr) return fail('nothing to compile on this line')
    const prev = elab.cs
    const session = this.#session as unknown as Session<Tree>
    elab.cs = session
    try {
      const lookup = (name: string): ScopeEntry | undefined => {
        const h = this.#byName.get(name)
        return h === undefined ? undefined : { tree: h as unknown as Tree }
      }
      const cir = exprToCir(
        expr,
        lookup,
        () => {
          throw new Error('`use` inside an inspected expression is not supported')
        }
      )
      const comb = eliminateLams(cir)
      const held = this.#heldCir(comb, d)
      const roots = [held]
      let closed = true
      const scan = (c: Cir): void => {
        if (c.tag === 'var') closed = false
        else if (c.tag === 'app') {
          scan(c.f)
          scan(c.x)
        } else if (c.tag === 'lam') scan(c.body)
      }
      scan(comb)
      // a def or test already has its value from the run; only free text needs computing
      if (spec.value != null) roots.push(d.add(spec.value))
      else if (closed) roots.push(d.add(cirToTree(comb) as unknown as number))
      return { graph: d.finish(roots), source: spec.expr ?? '' }
    } catch (e) {
      return fail(e instanceof Error ? e.message : String(e))
    } finally {
      elab.cs = prev
    }
  }

  /// The bracket lens's food: every intermediate term, binders still in place.
  bracket(spec: { expr?: string; path?: string; text?: string; line?: number; side?: 'lhs' | 'rhs'; maxNodes: number }): BracketDump {
    const d = this.#dumper(spec.maxNodes)
    const fail = (error: string): BracketDump => ({ graph: d.finish([]), steps: [], skipped: 0, mismatch: false, params: [], error })
    let expr: Expr | null = null
    try {
      expr = spec.expr != null ? parseExpr(spec.expr) : memberAt(spec.text ?? '', spec.line ?? 0, spec.side ?? 'lhs')
    } catch (e) {
      return fail(e instanceof Error ? e.message : String(e))
    }
    if (!expr) return fail('nothing to compile on this line')
    const prev = elab.cs
    elab.cs = this.#session as unknown as Session<Tree>
    try {
      const lookup = (name: string): ScopeEntry | undefined => {
        const h = this.#byName.get(name)
        return h === undefined ? undefined : { tree: h as unknown as Tree }
      }
      const cir = exprToCir(expr, lookup, () => {
        throw new Error('`use` inside an inspected expression is not supported')
      })
      const replay = bracketReplay(cir)
      const memo = new Map<Cir, number>()
      const clip = (s: string) => (s.length > 160 ? s.slice(0, 160) + '…' : s)
      const roots = replay.frames.map((f) => this.#heldCir(f.term, d, memo))
      const steps = replay.frames.map((f) => ({
        rule: f.rule,
        name: f.name,
        at: f.at,
        before: f.before ? clip(this.#cirText(f.before)) : '',
        after: f.after ? clip(this.#cirText(f.after)) : ''
      }))
      const params: string[] = []
      for (let e = cir; e.tag === 'lam'; e = e.body) params.push(e.x)
      return { graph: d.finish(roots), steps, skipped: replay.skipped, mismatch: replay.mismatch, params }
    } catch (e) {
      return fail(e instanceof Error ? e.message : String(e))
    } finally {
      elab.cs = prev
    }
  }

  // with `memo`, also draws what a replay frame can hold: binders, and K(x) kept visibly K
  #heldCir(root: Cir, d: { add: (h: number) => number; emit: (t: number, a: number, b: number) => number; str: (s: string) => number }, memo?: Map<Cir, number>): number {
    const leaf = () => d.emit(G_LEAF, 0, 0)
    const stem = (c: number) => d.emit(G_STEM, c, 0)
    const fork = (l: number, r: number) => d.emit(G_FORK, l, r)
    const go = (e: Cir): number => {
      if (!memo) return build(e)
      let id = memo.get(e)
      if (id === undefined) memo.set(e, (id = build(e)))
      return id
    }
    const build = (e: Cir): number => {
      switch (e.tag) {
        case 'lit':
          return d.add(e.t as unknown as number)
        case 'var':
          return d.emit(G_VAR, d.str(e.name), 0)
        case 'I':
          return fork(fork(leaf(), leaf()), leaf())
        case 'K':
          return stem(leaf())
        case 'S':
          return fork(stem(fork(leaf(), leaf())), leaf())
        case 'lam':
          if (!memo) throw new Error(`unexpected lambda for ${e.x}`)
          return d.emit(G_LAM, d.str(e.x), go(e.body))
        case 'app':
          if (e.f.tag === 'app' && e.f.f.tag === 'S') return fork(stem(go(e.f.x)), go(e.x))
          if (e.f.tag === 'app' && e.f.f.tag === 'K') return go(e.f.x)
          if (e.f.tag === 'K') return memo ? d.emit(G_HELD_K, go(e.x), 0) : fork(leaf(), go(e.x))
          if (e.f.tag === 'I') return go(e.x)
          if (e.f.tag === 'S') return stem(stem(go(e.x)))
          return d.emit(G_APP, go(e.f), go(e.x))
      }
    }
    return go(root)
  }

  #cirText(root: Cir): string {
    const atomText = (t: number): string => {
      const c = this.#session.classify(t)
      if (c.tag === 'leaf') return 't'
      const n = this.decodeNat(t)
      if (n !== null && n > 0) return String(n)
      const s = this.decodeString(t)
      if (s !== null) return JSON.stringify(s)
      return this.#labelOf(t) ?? this.#render(t, 2)
    }
    const go = (e: Cir, atom: boolean): string => {
      switch (e.tag) {
        case 'lit':
          return atomText(e.t as unknown as number)
        case 'var':
          return e.name
        case 'S':
        case 'K':
        case 'I':
          return e.tag
        case 'app': {
          const args = [e.x]
          let f = e.f
          while (f.tag === 'app') {
            args.unshift(f.x)
            f = f.f
          }
          return `${go(f, true)}(${args.map((a) => go(a, false)).join(', ')})`
        }
        case 'lam': {
          const params = [e.x]
          let b = e.body
          while (b.tag === 'lam') {
            params.push(b.x)
            b = b.body
          }
          const s = `{${params.join(', ')}} => ${go(b, false)}`
          return atom ? `(${s})` : s
        }
      }
    }
    const out = go(root, false)
    return out.length > 1800 ? out.slice(0, 1800) + '…' : out
  }

  // Call-style pretty printer (t(x, t(y)) …), name-aware, capped.
  pretty(h: number, selfName?: string): string {
    const out = this.#render(h, PRETTY_DEPTH, selfName)
    return out.length > PRETTY_LEN ? out.slice(0, PRETTY_LEN) + '…' : out
  }

  #render(h: number, depth: number, selfName?: string): string {
    // Display priority: the leaf is always 't' (the atom everyone knows);
    // then numeral/string decodings (many REPL names alias small trees —
    // `succ zero` and a compiled `{n} => refl` are literally the same
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
    if (c.tag === 'stem') return `t(${this.#render(c.child, depth - 1)})`
    return `t(${this.#render(c.left, depth - 1)}, ${this.#render(c.right, depth - 1)})`
  }
}
