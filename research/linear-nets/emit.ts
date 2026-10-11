// EXPERIMENTAL (research/LINEAR_TREE_NETS.md). Compiles disp calls as the strand player does, with the
// outermost call's arguments worked out first, and prints one line a call for `linear-nets`:
//   name|head|arg,arg,...|answer      (head, args and answer as ternary: 0 leaf, 1x stem, 2xy fork)
// The head is the compiled function (code), the args are data; the answer is the eager evaluator's.
//   npx tsx research/linear-nets/emit.ts [--let 'name := ...'] '<call>' ... > calls.txt
import { mkdtempSync, readFileSync, writeFileSync } from "node:fs"
import { tmpdir } from "node:os"
import { dirname, join, resolve } from "node:path"
import { fileURLToPath } from "node:url"
import { parseProgram } from "../../src/compile.js"
import type { Session } from "../../src/eval/types.js"
import type { Tree } from "../../src/eval/eager.js"
import { StrandsCompiler, SCOPE } from "../../evaluators/rust-ic-strands/programs/compiler.js"

const repo = resolve(dirname(fileURLToPath(import.meta.url)), "../..")
const NODE = 0x8000_0000
const lets: string[] = [], calls: string[] = []
const argv = process.argv.slice(2)
for (let i = 0; i < argv.length; i++) argv[i] === "--let" ? lets.push(argv[++i]) : calls.push(argv[i])

const c = new StrandsCompiler(readFileSync(join(repo, "website/static/rust_eager.wasm")), repo, p => readFileSync(p, "utf-8"))
const s = c.session
const uses = SCOPE.map(f => `${repo}/${f}`)
if (lets.length) {
  const file = join(mkdtempSync(join(tmpdir(), "linear-nets-")), "lets.disp")
  writeFileSync(file, uses.map(f => `open use raw "${f}" {}\n`).join("") + lets.join("\n") + "\n")
  uses.push(file)
}
const opens = uses.map(f => `open use raw "${f}" {}\n`).join("")
const input = join(repo, "evaluators/rust-ic-strands/programs/input.disp")
if (lets.length) parseProgram(opens, input, { session: s as unknown as Session<Tree> })

for (const src of calls) {
  let tree: number | undefined
  s.defer = true
  try {
    for (const d of parseProgram(`${opens}__it := (${src})\n`, input, { session: s as unknown as Session<Tree> }))
      if (d.kind === "Def" && d.name === "__it") tree = d.tree as unknown as number
  } finally { s.defer = false }
  if (tree === undefined) throw new Error(`${src}: did not compile`)
  const spine: number[] = []
  let head = tree
  while (head >= NODE && s.nodes[head - NODE].op === "@") { spine.unshift(s.nodes[head - NODE].b); head = s.nodes[head - NODE].a }
  const args = spine.map(a => s.ternary(s.force(a)))
  const answer = s.ternary(spine.reduce((f, a) => s.s.apply(f, s.force(a)), s.force(head)))
  console.log(`${src}|${s.ternary(head)}|${args.join(",")}|${answer}`)
}
