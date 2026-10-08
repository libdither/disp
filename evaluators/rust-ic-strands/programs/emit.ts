// Compile programs.disp with the real elaborator and write every documented definition's tree
// to ../player/programs.js, so the player runs ordinary disp code. Run from anywhere:
//   npx tsx evaluators/rust-ic-strands/programs/emit.ts
import { readFileSync, writeFileSync } from "node:fs"
import { dirname, join } from "node:path"
import { fileURLToPath } from "node:url"
import { parseProgram } from "../../../src/compile.js"
import { getBackend, defaultBackendName } from "../../../src/eval/registry.js"
import { emitBlob } from "../../../src/format/export.js"

const here = dirname(fileURLToPath(import.meta.url))
const file = join(here, "programs.disp")
const src = readFileSync(file, "utf-8")
const session = getBackend(defaultBackendName).createSession()
const trees = new Map<string, unknown>()
for (const d of parseProgram(src, file, { session })) if (d.kind === "Def") trees.set(d.name, d.tree)

// `/// <what> -> <kind>; try: <args>` documents the definition on the next line; `bits16` is binary
// numbers of a fixed width, 16 bits.
const lines = src.split("\n")
const programs = []
for (let i = 0; i + 1 < lines.length; i++) {
  const doc = lines[i].match(/^\/\/\/ (.*) -> (nat|bool|list|string|bits)(\d*); try: (.*)$/)
  const def = lines[i + 1].match(/^(\w+) :=/)
  if (!doc || !def) continue
  const tree = trees.get(def[1])
  if (tree === undefined) throw new Error(`${def[1]} did not compile`)
  programs.push({ name: def[1], desc: doc[1], kind: doc[2], ...(doc[3] ? { width: +doc[3] } : {}), example: doc[4].trim(), tree: emitBlob(session, tree as never).trim() })
}
writeFileSync(join(here, "../player/programs.js"),
  "// generated from programs/programs.disp by programs/emit.ts; do not edit\nwindow.DISP_PROGRAMS = "
  + JSON.stringify(programs, null, 1) + ";\n")
console.log(`player/programs.js: ${programs.map(p => `${p.name} (${p.tree.length})`).join(", ")}`)
