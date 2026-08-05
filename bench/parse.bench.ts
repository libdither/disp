// Parser-only corpus benchmarks. The corpus is discovered dynamically so new,
// moved, and optimized Disp sources are measured without maintaining a file list.
// Run with `npx vitest bench bench/parse.bench.ts --run`.

import { bench, describe } from "vitest"
import { readFileSync, readdirSync } from "node:fs"
import { dirname, join, relative, resolve } from "node:path"
import { fileURLToPath } from "node:url"
import { parseItems, tokenize } from "../src/parse.js"

const ROOT = resolve(dirname(fileURLToPath(import.meta.url)), "..")

function dispFiles(dir: string): string[] {
  return readdirSync(dir, { withFileTypes: true })
    .flatMap(entry => entry.isDirectory()
      ? dispFiles(join(dir, entry.name))
      : entry.name.endsWith(".disp") ? [join(dir, entry.name)] : [])
    .sort()
}

const sources = dispFiles(join(ROOT, "lib")).map(path => ({
  name: relative(ROOT, path),
  src: readFileSync(path, "utf8"),
}))
const bytes = sources.reduce((total, source) => total + Buffer.byteLength(source.src), 0)
const corpus = `${sources.length} files / ${(bytes / 1024).toFixed(1)} KiB`

describe("parser corpus", () => {
  bench(`tokenize ${corpus}`, () => {
    for (const { src } of sources) tokenize(src)
  })

  bench(`parseItems ${corpus}`, () => {
    for (const { src } of sources) parseItems(src)
  })
})
