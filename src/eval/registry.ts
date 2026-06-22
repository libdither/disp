// Backend registry: name -> EvalBackend, for `--evaluator=<name>` resolution
// (EVALUATOR_PLAN §5). The eager reference backend is always registered; other
// backends (the Phase-3 naive conformance backend, a future Rust/WASM TC-Net
// runtime) register themselves here.

import type { EvalBackend } from "./types.js"
import { eagerBackend } from "./eager.js"
import { naiveBackend } from "./naive.js"

// Handles are opaque to the engine, so backends are stored at `unknown` handle
// type; each backend casts internally (the eager/naive backends' H = Tree).
const backends = new Map<string, EvalBackend>([
  [eagerBackend.name, eagerBackend as unknown as EvalBackend],
  [naiveBackend.name, naiveBackend as unknown as EvalBackend],
])

export const defaultBackendName = "eager"

export function registerBackend(b: EvalBackend): void {
  backends.set(b.name, b)
}

export function getBackend(name: string): EvalBackend {
  const b = backends.get(name)
  if (!b) throw new Error(`unknown evaluator backend '${name}' (have: ${[...backends.keys()].join(", ")})`)
  return b
}

export function backendNames(): string[] {
  return [...backends.keys()]
}
