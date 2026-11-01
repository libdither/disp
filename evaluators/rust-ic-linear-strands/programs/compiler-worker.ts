// The player's compiler worker, bundled into player/compiler.js by bundle.ts. It answers
// { id, src } with { id, result } (compiler.ts `Compiled`) or { id, error }, after saying
// { ready, names } once the scope has loaded.
import { readFileSync } from "node:fs"
import { StrandsCompiler } from "./compiler.js"

/// website/static/rust_eager.wasm in base64, put here by bundle.ts.
declare const RUST_EAGER_WASM: string

const post = (m: unknown) => (self as unknown as Worker).postMessage(m)
let compiler: StrandsCompiler
try {
  const t0 = performance.now()
  compiler = new StrandsCompiler(Uint8Array.from(atob(RUST_EAGER_WASM), c => c.charCodeAt(0)), "", p => readFileSync(p, "utf-8"))
  post({ ready: true, names: compiler.names, ms: performance.now() - t0 })
} catch (e) {
  post({ fatal: e instanceof Error ? e.message : String(e) })
}
self.onmessage = (e: MessageEvent<{ id: number; src: string }>) => {
  const { id, src } = e.data
  try { post({ id, result: compiler.compile(src) }) }
  catch (err) { post({ id, error: err instanceof Error ? err.message : String(err) }) }
}
