// Benchmark disp expressions on the strand lattice. Each expression is compiled as the player compiles
// it (compiler.ts: reductions left undone, so the lattice does the work), run to the answer on the
// current design (`latest`) on a lattice sized as the player sizes it (crate/src/bin/strands-bench.rs),
// one process a seed, at most --jobs at once, each memory-capped and timed out; the answer is checked
// against the eager evaluator. Run from anywhere:
//   npx tsx evaluators/rust-ic-strands/programs/bench.ts [options] '<expr>' ...
//     --seeds N        seeds a run (2)                 --jobs N        processes at once (4)
//     --mem SIZE       memory cap a process (1500M)    --timeout S     seconds a run (3600)
//     --clocks N       give up after N clocks
//     --data           work out the outermost call's arguments first, eagerly: inputs as data
//     --use FILE       a .disp file of extra definitions in scope (it opens what it needs itself)
//     --let 'D'        a definition in scope, worked out while compiling: --let 'add8 := relay_for (bit_tree 3 0)'
//     --set k=v        a lattice setting, as strands-run spells it    --json FILE   append the runs
//     --kind K         read answers as K (nat, bits) where the program does not say (a `///` line)
//     --adders LIST    the adder suite (implies --data): name:shape,... with shape unary (to 8 bits), list or tree,
//     --widths LIST    at these widths (2,4,8,16), on --inputs max (2^n - 1 plus 1) and/or mixed
//                      (51566 + 22995 cut to n bits)
//     --no-lattice     compile and run eagerly only
//     --ideal          the idealised lazy machine instead of the lattice (memo.rs: every wanted rewrite at
//                      once): work (rewrites and collections) and depth (rounds), in no time
// Columns: code (nodes of the compiled function, or of the whole term), agents at load, eager steps
// (rust-eager interactions in a fresh session, beyond loading the term), clocks, rewrites and the
// shares of them by duplicators and erasers, peak strands and sites, CPU seconds a run (means over
// seeds; clocks also as min-max).
import { mkdtempSync, readFileSync, writeFileSync, appendFileSync } from "node:fs"
import { tmpdir } from "node:os"
import { dirname, join, resolve } from "node:path"
import { fileURLToPath } from "node:url"
import { spawn, spawnSync } from "node:child_process"
import { parseProgram } from "../../../src/compile.js"
import type { Session } from "../../../src/eval/types.js"
import type { Tree } from "../../../src/eval/eager.js"
import { RustEagerBrowserSession } from "../../../website/src/lib/disp/rust-eager-browser.js"
import { StrandsCompiler, SCOPE } from "./compiler.js"

const here = dirname(fileURLToPath(import.meta.url))
const repo = resolve(here, "../../..")
const crate = join(here, "../crate")
const NODE = 0x8000_0000

const argv = process.argv.slice(2)
const opt = { seeds: 2, jobs: 4, mem: "1500M", timeout: 3600, clocks: 0, data: false, lattice: true, ideal: false,
  kind: "", uses: [] as string[], lets: [] as string[], sets: [] as string[], json: "", adders: "", widths: "2,4,8,16", inputs: "max,mixed" }
const exprs: string[] = []
for (let i = 0; i < argv.length; i++) {
  const a = argv[i], next = () => argv[++i]
  switch (a) {
    case "--seeds": opt.seeds = +next(); break
    case "--jobs": opt.jobs = +next(); break
    case "--mem": opt.mem = next(); break
    case "--timeout": opt.timeout = +next(); break
    case "--clocks": opt.clocks = +next(); break
    case "--data": opt.data = true; break
    case "--use": opt.uses.push(resolve(next())); break
    case "--let": opt.lets.push(next()); break
    case "--kind": opt.kind = next(); break
    case "--set": opt.sets.push(next()); break
    case "--json": opt.json = next(); break
    case "--adders": opt.adders = next(); opt.data = true; break
    case "--widths": opt.widths = next(); break
    case "--inputs": opt.inputs = next(); break
    case "--no-lattice": opt.lattice = false; break
    case "--ideal": opt.ideal = true; opt.seeds = 1; break
    default: exprs.push(a)
  }
}

/// The adder suite's expressions: each adder at each width and input.
function suite(): string[] {
  const out: string[] = []
  for (const w of opt.widths.split(",").map(Number)) for (const input of opt.inputs.split(",")) {
    const [x, y] = input === "max" ? [2 ** w - 1, 1] : [51566 % 2 ** w, 22995 % 2 ** w]
    for (const spec of opt.adders.split(",")) {
      const [name, shape = "tree"] = spec.split(":")
      if (shape === "unary") { if (w <= 8) out.push(`${name} ${x} ${y}`) }
      else if (shape === "list") out.push(`${name} (bits ${w} ${x}) (bits ${w} ${y})`)
      else out.push(`${name} (bit_tree ${Math.log2(w)} ${x}) (bit_tree ${Math.log2(w)} ${y})`)
    }
  }
  return out
}
if (opt.adders) exprs.push(...suite())
if (!exprs.length) { console.error("usage: bench.ts [options] '<disp expression>' ... (see the top of bench.ts)"); process.exit(1) }

// ---- compiling --------------------------------------------------------------------------------
if (opt.lets.length) {
  const file = join(mkdtempSync(join(tmpdir(), "strands-bench-")), "lets.disp")
  writeFileSync(file, SCOPE.map(f => `open use raw "${repo}/${f}" {}\n`).join("") + opt.lets.join("\n") + "\n")
  opt.uses.push(file)
}
const wasm = readFileSync(join(repo, "website/static/rust_eager.wasm"))
const c = new StrandsCompiler(wasm, repo, p => readFileSync(p, "utf-8"))
const s = c.session
const opens = [...SCOPE.map(f => `${repo}/${f}`), ...opt.uses].map(f => `open use raw "${f}" {}\n`).join("")
const input = join(here, "input.disp")
// Load the --use files now, with reductions done, so their definitions are values as programs.disp's are.
if (opt.uses.length) parseProgram(opens, input, { session: s as unknown as Session<Tree> })

/// The kind each documented program returns (`/// … -> bits; try: …`).
const kinds = new Map<string, string>()
for (const f of [join(here, "programs.disp"), ...opt.uses]) {
  const lines = readFileSync(f, "utf-8").split("\n")
  lines.forEach((l, i) => {
    const k = l.match(/^\/\/\/ .* -> (nat|bool|list|string|bits)\d*(;|$)/)?.[1], d = lines[i + 1]?.match(/^(\w+) :=/)?.[1]
    if (k && d) kinds.set(d, k)
  })
}

/// The expression as a deferred handle: applications left undone.
function compileHandle(src: string): number {
  let tree: number | undefined
  s.defer = true
  try {
    for (const d of parseProgram(`${opens}__it := (${src})\n`, input, { session: s as unknown as Session<Tree> }))
      if (d.kind === "Def" && d.name === "__it") tree = d.tree as unknown as number
  } finally { s.defer = false }
  if (tree === undefined) throw new Error(`${src}: did not compile`)
  return tree
}

type Case = { src: string; term: string; code: number; kind?: string; want: string; steps: number }

/// A value's ternary digits in a session, read node by node.
function ternaryOf(e: RustEagerBrowserSession, h: number): string {
  const out: string[] = [], stack = [h]
  while (stack.length) {
    const k = e.classify(stack.pop()!)
    if (k.tag === "leaf") out.push("0")
    else if (k.tag === "stem") { out.push("1"); stack.push(k.child) }
    else { out.push("2"); stack.push(k.right, k.left) }
  }
  return out.join("")
}

function prepare(src: string): Case {
  const h = compileHandle(src)
  // The call's head and arguments; with --data each argument is worked out now, as a literal.
  const spine: number[] = []
  let head = h
  while (head >= NODE && s.nodes[head - NODE].op === "@") { spine.unshift(s.nodes[head - NODE].b); head = s.nodes[head - NODE].a }
  if (opt.data) for (let i = 0; i < spine.length; i++) spine[i] = s.force(spine[i])
  const term = opt.data ? [s.term(head), ...spine.map(a => s.ternary(a))].join(" ") : s.term(h)
  const code = head < NODE ? s.ternary(head).length : term.length
  // The answer and its cost on the eager evaluator, from a fresh session (no memo).
  const e = new RustEagerBrowserSession(wasm)
  const loaded = new Map<number, number>()
  const load = (x: number): void => {
    if (x < NODE) { if (!loaded.has(x)) loaded.set(x, e.loadTernary(s.ternary(x))); return }
    const n = s.nodes[x - NODE]; load(n.a); if (n.op !== "S") load(n.b)
  }
  const run = (x: number): number => {
    if (x < NODE) return loaded.get(x)!
    const n = s.nodes[x - NODE]
    return n.op === "@" ? e.apply(run(n.a), run(n.b)) : n.op === "S" ? e.stem(run(n.a)) : e.fork(run(n.a), run(n.b))
  }
  load(head); spine.forEach(load)
  const before = e.stats().steps
  const value = spine.reduce((f, a) => e.apply(f, run(a)), run(head))
  const steps = e.stats().steps - before
  const want = ternaryOf(e, value)
  e.dispose()
  const name = src.match(/^\s*(\w+)/)?.[1]
  return { src, term, code, kind: (name && kinds.get(name)) || opt.kind || undefined, want, steps }
}

// ---- reading answers ----------------------------------------------------------------------------
type T = { k: 0 } | { k: 1; c: T } | { k: 2; l: T; r: T }
function parseTernary(t: string): T {
  let i = 0
  const go = (): T => { const d = t[i++]; return d === "0" ? { k: 0 } : d === "1" ? { k: 1, c: go() } : { k: 2, l: go(), r: go() } }
  return go()
}
function bitsOf(t: T): { v: bigint; w: bigint } | null {
  if (t.k === 0) return { v: 0n, w: 1n }
  if (t.k === 1) return t.c.k === 0 ? { v: 1n, w: 1n } : null
  const lo = bitsOf(t.l), hi = lo && bitsOf(t.r)
  return hi && { v: lo!.v + (hi.v << lo!.w), w: lo!.w + hi.w }
}
function show(kind: string | undefined, ternary: string): string {
  if (!ternary) return "-"
  if (kind === "bits") { const b = bitsOf(parseTernary(ternary)); if (b) return String(b.v) }
  if (/^(20)*0$/.test(ternary) && kind !== "bits") return String((ternary.length - 1) / 2)
  return ternary.length > 24 ? ternary.slice(0, 21) + "…" : ternary
}

// ---- running ------------------------------------------------------------------------------------
type Run = { name: string; seed: number; side: number; agents: number; done: boolean; clocks: number; rewrites: number; collected: number;
  peak_strands: number; peak_sites: number; wall: number; depth: number; rules: Record<string, number>; answer: string; error?: string }

const cases = exprs.map(prepare)
for (const k of cases) console.error(`${k.src}: ${k.code} nodes, eager ${k.steps} steps, answer ${show(k.kind, k.want)}`)

async function lattice(): Promise<Map<Case, Run[]>> {
  const build = spawnSync("systemd-run", ["--user", "--scope", "-q", "-p", "MemoryMax=4G", "-p", "MemorySwapMax=256M", "timeout", "900",
    "cargo", "build", "--release", "-q", "--bin", "strands-bench"], { cwd: crate, stdio: "inherit" })
  if (build.status !== 0) throw new Error("strands-bench did not build")
  const bin = join(crate, "target/release/strands-bench"), dir = mkdtempSync(join(tmpdir(), "strands-bench-"))
  const jobs = cases.flatMap((k, i) => {
    const file = join(dir, `${i}.txt`)
    writeFileSync(file, `${i}|${k.term}\n`)
    return Array.from({ length: opt.seeds }, (_, j) => ({ k, file, seed: j + 1 }))
  })
  const runs = new Map<Case, Run[]>(cases.map(k => [k, []]))
  let next = 0
  const worker = async () => {
    while (next < jobs.length) {
      const { k, file, seed } = jobs[next++]
      const args = ["--user", "--scope", "-q", "-p", `MemoryMax=${opt.mem}`, "-p", "MemorySwapMax=256M", "timeout", String(opt.timeout),
        bin, file, "--seed", String(seed), "--rules", ...(opt.ideal ? ["--ideal"] : []), ...(opt.clocks ? ["--clocks", String(opt.clocks)] : []), ...opt.sets]
      const out = await new Promise<string>(ok => {
        const p = spawn("systemd-run", args, { stdio: ["ignore", "pipe", "inherit"] })
        let text = ""
        p.stdout.on("data", d => text += d)
        p.on("close", code => ok(code === 0 ? text : JSON.stringify({ error: code === 124 ? "timed out" : `exit ${code}` })))
      })
      const r: Run = { side: 0, clocks: NaN, depth: NaN, peak_strands: NaN, peak_sites: NaN, wall: NaN, ...JSON.parse(out.trim().split("\n").pop() || "{}"), seed }
      r.name = k.src
      runs.get(k)!.push(r)
      const ok = r.error ? r.error : !r.done ? "UNFINISHED" : r.answer === k.want ? "ok" : `WRONG ${show(k.kind, r.answer)}`
      console.error(`  ${k.src} seed ${seed}: ${r.clocks ?? "-"} clocks, ${r.rewrites ?? "-"} rewrites, ${r.wall ?? "-"} s, ${ok}`)
      if (opt.json) appendFileSync(opt.json, JSON.stringify({ ...r, code: k.code, steps: k.steps, ok: ok === "ok" }) + "\n")
    }
  }
  await Promise.all(Array.from({ length: opt.jobs }, worker))
  return runs
}

const kfmt = (x: number) => isNaN(x) ? "-" : x >= 9950 ? `${(x / 1000).toFixed(1)}k` : x.toFixed(0)
const mean = (xs: number[]) => xs.reduce((a, b) => a + b, 0) / Math.max(1, xs.length)
const rows: string[][] = [["expression", "code", "agents", "eager", opt.ideal ? "depth" : "clocks", "(min-max)", "rewrites", "Dn %", "Eps %", "strands", "sites", "wall s", "answer"]]
const runs = opt.lattice ? await lattice() : new Map<Case, Run[]>()
for (const k of cases) {
  const rs = runs.get(k) ?? [], good = rs.filter(r => !r.error && r.done && r.answer === k.want)
  const m = (f: (r: Run) => number) => good.length ? kfmt(mean(good.map(f))) : "-"
  const cl = good.map(r => r.clocks)
  const verdict = !opt.lattice ? show(k.kind, k.want) : good.length === rs.length ? `${show(k.kind, k.want)} ok` : `${good.length}/${rs.length} ok`
  const row = [k.src, String(k.code), good[0] ? String(good[0].agents) : "-", kfmt(k.steps), m(r => opt.ideal ? r.depth : r.clocks),
    cl.length && !opt.ideal ? `${kfmt(Math.min(...cl))}-${kfmt(Math.max(...cl))}` : "-", m(r => r.rewrites),
    ...["Dn", "Eps"].map(tag => good.length && !opt.ideal ? (100 * mean(good.map(r => (r.rules[tag] ?? 0) / r.rewrites))).toFixed(0) : "-"),
    m(r => r.peak_strands), m(r => r.peak_sites), good.length && !opt.ideal ? mean(good.map(r => r.wall)).toFixed(1) : "-", verdict]
  rows.push(row)
}
const widths = rows[0].map((_, i) => Math.max(...rows.map(r => r[i].length)))
for (const r of rows) console.log(r.map((x, i) => i === 0 ? x.padEnd(widths[i]) : x.padStart(widths[i])).join("  "))
