// Parser benchmarks — run with `vitest bench` or `npm run bench`.
// Skipped during normal `vitest run` / `npm test`.

import { describe, bench } from "vitest"
import { readFileSync } from "node:fs"
import { resolve } from "node:path"
import { tokenize, parseItems } from "../src/parse.js"
import { parseProgram } from "../src/compile.js"

const FILES = [
  "lib/prelude.disp",
  "lib/kernel/core.disp",
  "lib/std/list.disp",
  "lib/tests/prelude.test.disp",
  "lib/tests/kernel.test.disp",
  "lib/tests/types.test.disp",
]

const sources = FILES.map(f => ({
  name: f,
  src: readFileSync(resolve(f), "utf-8"),
  path: resolve(f),
}))

describe("tokenize", () => {
  for (const { name, src } of sources) {
    bench(name, () => { tokenize(src) })
  }
})

describe("parseItems", () => {
  for (const { name, src } of sources) {
    bench(name, () => { parseItems(src) })
  }
})

describe("parseProgram", () => {
  for (const { name, src, path } of sources) {
    bench(name, () => { parseProgram(src, path) })
  }
})
