// The disp compiler behind the player's input box: an expression, with the prelude, lib/list.disp
// and programs.disp in scope, becomes a term for the lattice. The elaborator would evaluate
// `fib 3` to 2 on the spot; here its reductions are left undone as applications, so the lattice
// does the work. Runs in the player's worker (compiler-worker.ts) and under node for tests.
import { parseProgram } from "../../../src/compile.js"
import { parseExpr, parseItems, type Expr } from "../../../src/parse.js"
import { moduleCacheBySession } from "../../../src/elab/state.js"
import type { Budget, Classification, Session } from "../../../src/eval/types.js"
import type { Tree } from "../../../src/eval/eager.js"
import { RustEagerBrowserSession } from "../../../website/src/lib/disp/rust-eager-browser.js"

/// Handles at or above this are application, stem or fork nodes kept here, not in the arena.
const NODE = 0x8000_0000
type Node = { op: "@" | "S" | "F"; a: number; b: number; v?: number }

/// A session that, while `defer` is on, leaves every reduction undone: applying a fork gives an
/// application node. Applying a leaf or a stem only builds a tree, so that still happens. Anything
/// that looks inside a node (classify, equal, dump) evaluates it first, so the elaborator sees
/// the same values as ever.
export class DeferringSession implements Session<number> {
  readonly canonicalHandles = false
  defer = false
  /// Functions applied at once even while deferring (succ, which spells numerals).
  readonly eager = new Set<number>()
  readonly nodes: Node[] = []
  constructor(readonly s: RustEagerBrowserSession) {}

  #node(op: Node["op"], a: number, b = 0): number { this.nodes.push({ op, a, b }); return NODE + this.nodes.length - 1 }
  force(h: number): number {
    if (h < NODE) return h
    const n = this.nodes[h - NODE]
    n.v ??= n.op === "@" ? this.s.apply(this.force(n.a), this.force(n.b))
      : n.op === "S" ? this.s.stem(this.force(n.a)) : this.s.fork(this.force(n.a), this.force(n.b))
    return n.v
  }
  leaf(): number { return this.s.leaf() }
  stem(c: number): number { return c >= NODE ? this.#node("S", c) : this.s.stem(c) }
  fork(l: number, r: number): number { return l >= NODE || r >= NODE ? this.#node("F", l, r) : this.s.fork(l, r) }
  apply(f: number, x: number, budget?: Budget): number {
    if (this.defer && f < NODE) {
      const c = this.s.classify(f)
      if (c.tag === "leaf") return this.stem(x)
      if (c.tag === "stem") return this.fork(c.child, x)
      if (this.eager.has(f) && x < NODE) return this.s.apply(f, x, budget)
    }
    if (this.defer) return this.#node("@", f, x)
    return this.s.apply(this.force(f), this.force(x), budget)
  }
  loadTernary(t: string): number { return this.s.loadTernary(t) }
  dumpTernary(h: number): string { return this.ternary(h) }
  /// A value's ternary digits, read node by node: the arena's own dump frees its buffer with the
  /// wrong size and panics.
  ternary(h: number, limit = 4_000_000): string {
    const out: string[] = [], stack = [this.force(h)]
    while (stack.length) {
      const c = this.s.classify(stack.pop()!)
      if (c.tag === "leaf") out.push("0")
      else if (c.tag === "stem") { out.push("1"); stack.push(c.child) }
      else { out.push("2"); stack.push(c.right, c.left) }
      if (out.length > limit) throw new Error(`the term is too big for the lattice (over ${limit} nodes)`)
    }
    return out.join("")
  }
  equal(a: number, b: number, budget?: Budget): boolean { return this.s.equal(this.force(a), this.force(b), budget) }
  classify(h: number, budget?: Budget): Classification<number> { return this.s.classify(this.force(h), budget) }
  stats() { return this.s.stats() }
  recognizeNative(name: string, h: number): void { this.s.recognizeNative(name, this.force(h)) }
  dispose(): void { this.s.dispose() }

  /// The term in the engine's notation: ternary trees applied left to right when it is a plain
  /// call, else L, S(x), F(x,y) and @(f,x).
  term(h: number, limit = 4_000_000): string {
    const spine: number[] = []
    let head = h
    while (head >= NODE && this.nodes[head - NODE].op === "@") { spine.unshift(this.nodes[head - NODE].b); head = this.nodes[head - NODE].a }
    if (head < NODE && spine.every(x => x < NODE)) return [head, ...spine].map(x => this.ternary(x, limit)).join(" ")
    const seen = new Map<number, string>()
    let size = 0
    const go = (h: number): string => {
      if (h >= NODE) {
        const n = this.nodes[h - NODE]
        return n.op === "S" ? `S(${go(n.a)})` : `${n.op}(${go(n.a)},${go(n.b)})`
      }
      let t = seen.get(h)
      if (t === undefined) { t = ternaryToTerm(this.ternary(h, limit)); seen.set(h, t) }
      if ((size += t.length) > limit) throw new Error(`the term is too big for the lattice (over ${limit} characters)`)
      return t
    }
    return go(h)
  }
}

/// `0` leaf, `1x` stem, `2xy` fork, in the engine's L, S(x), F(x,y).
function ternaryToTerm(t: string): string {
  let out = ""
  const close: string[] = []
  for (const d of t) {
    out += d === "0" ? "L" : d === "1" ? "S(" : "F("
    if (d === "1") close.push(")")
    else if (d === "2") close.push(")", ",")
    else while (close.length) { const c = close.pop()!; out += c; if (c === ",") break }
  }
  return out
}

export type Compiled = { term: string; head?: string; kind?: string }

/// The kind of value a declared type returns (`List A -> Nat` returns a nat), and how many
/// arguments it takes first.
function declared(type: Expr): { args: number; kind?: string } {
  let args = 0
  while (type.tag === "binder" && !type.fat) { args += Math.max(1, type.params.length); type = type.body }
  let head = type
  while (head.tag === "app") head = head.f
  const kind = head.tag !== "var" ? undefined
    : head.name === "Nat" ? "nat" : head.name === "Bool" ? "bool" : head.name === "String" ? "string" : head.name === "List" ? "list" : undefined
  return { args, kind }
}

/// The function an application calls, and how many arguments it gets (`xs.map(f)` calls map with two).
function callOf(e: Expr, args = 0): { name: string; args: number } | undefined {
  if (e.tag === "app") return callOf(e.f, args + 1)
  if (e.tag === "ann") return callOf(e.expr, args)
  if (e.tag === "var") return { name: e.name, args }
  if (e.tag === "proj") return { name: e.field, args: args + 1 }
  return undefined
}

/// The modules in scope, from the repository's root; expressions are compiled in a file next to
/// programs.disp.
export const SCOPE = ["lib/prelude.disp", "lib/list.disp", "evaluators/rust-ic-strands/programs/programs.disp"]
const INPUT = "evaluators/rust-ic-strands/programs/input.disp"

export class StrandsCompiler {
  readonly session: DeferringSession
  /// Every top-level definition in scope, as ternary trees.
  readonly names: { name: string; tree: string }[] = []
  readonly #kinds = new Map<string, { args: number; kind?: string }>()
  readonly #input: string
  readonly #opens: string

  /// `repo` is where the repository is ("" in the browser's virtual filesystem), `read` reads a file.
  constructor(wasm: ArrayBuffer | Uint8Array, repo: string, read: (path: string) => string) {
    this.session = new DeferringSession(new RustEagerBrowserSession(wasm))
    this.#input = `${repo}/${INPUT}`
    this.#opens = SCOPE.map(f => `open use raw "${repo}/${f}" {}\n`).join("")
    parseProgram(this.#opens, this.#input, { session: this.session as unknown as Session<Tree> })
    const cache = moduleCacheBySession.get(this.session as unknown as Session<Tree>)!
    const seen = new Set<string>()
    for (const f of SCOPE.map(f => `${repo}/${f}`)) {
      const entry = [...cache].find(([k]) => k.split("\0")[0] === f)?.[1]
      if (!entry?.fields || !entry.fieldTrees) throw new Error(`${f} did not load`)
      entry.fields.forEach((name, i) => {
        if (seen.has(name) || name.startsWith("__")) return
        seen.add(name)
        const tree = entry.fieldTrees![i] as unknown as number
        this.names.push({ name, tree: this.session.ternary(tree) })
        if (name === "succ") this.session.eager.add(tree)
      })
      for (const it of parseItems(read(f)))
        if (it.tag === "field" && (it.type ?? it.docType)) this.#kinds.set(it.name, declared((it.type ?? it.docType)!))
    }
  }

  compile(src: string): Compiled {
    const expr = parseExpr(src)
    const s = this.session
    let tree: number | undefined
    s.defer = true
    try {
      for (const d of parseProgram(`${this.#opens}__it := (${src})\n`, this.#input, { session: s as unknown as Session<Tree> }))
        if (d.kind === "Def" && d.name === "__it") tree = d.tree as unknown as number
    } catch (e) {
      const free = String((e as Error).message ?? e).match(/unresolved free variable (\S+)/)
      throw free ? new Error(`${free[1]} is not defined (in scope: the prelude, lib/list.disp and programs.disp)`) : e
    } finally { s.defer = false }
    if (tree === undefined) throw new Error("the expression did not compile")
    const call = callOf(expr), known = call && this.#kinds.get(call.name)
    return { term: s.term(tree), head: call?.name, kind: known && known.args === call.args ? known.kind : undefined }
  }
}
