// The in-worker disp engine: wraps the real elaborator (src/compile.ts) around
// a rust-eager WASM session, mirroring src/run.ts's runFile semantics, plus
// REPL niceties (value pretty-printing, nat/string decoding, streamed
// per-item progress).

import { parseProgram, type Decl, type ParseItemStats } from '../../../../src/compile.ts'
import type { Session } from '../../../../src/eval/types.ts'
import type { Tree } from '../../../../src/eval/eager.ts'
import { RustEagerBrowserSession } from './rust-eager-browser.ts'
import { restoreSnapshot, vfsHash, type RestoreStats } from './snapshot.ts'
import { vfs } from './shims/fs.ts'
import type { ItemEvent, RunOutcome, ValueNode } from './protocol.ts'

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
    // scaffolding (the REPL's __it) stays out of the name table.
    for (const [name, d] of rootDefs)
      if (!name.startsWith('__') && !this.#names.has(d.handle)) this.#names.set(d.handle, name)
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
