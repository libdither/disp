// Backend registry: name -> EvalBackend, for `--evaluator=<name>` resolution
// (EVALUATOR_PLAN §5). The eager reference backend is always registered; other
// backends (the Phase-3 naive conformance backend, a future Rust/WASM net
// runtime) register themselves here.

import type { EvalBackend } from "./types.js"
import { eagerBackend } from "./eager.js"
import { naiveBackend } from "./naive.js"
import { rustEagerBackend, rustEagerAvailable } from "./rust-eager.js"
import { icNetBackend, icNetAvailable } from "./ic-net.js"

// Handles are opaque to the engine, so backends are stored at `unknown` handle
// type; each backend casts internally (the eager/naive backends' H = Tree).
const backends = new Map<string, EvalBackend>([
  [eagerBackend.name, eagerBackend as unknown as EvalBackend],
  [naiveBackend.name, naiveBackend as unknown as EvalBackend],
])

// Foreign in-process backends register only if their build artifact exists, so
// the everyday loop needs no foreign toolchain (EVALUATOR_LAYOUT.md). rust-eager is
// a Rust→WASM Session backend; absent until evaluators/rust-eager/build.sh runs.
if (rustEagerAvailable()) backends.set(rustEagerBackend.name, rustEagerBackend as unknown as EvalBackend)

// rust-ic-net: the materialized interaction-net backend (M0). Registers when built,
// but is NEVER the default — it's a batch/differential + optimizer substrate, not the
// checker (no hash-consing ⇒ no O(1) conversion). See RUST_IC_NET_DESIGN.md.
if (icNetAvailable()) backends.set(icNetBackend.name, icNetBackend as unknown as EvalBackend)

// Default to rust-eager when its WASM artifact is built: the Rust reducer uses linear
// memory (not the V8 heap), so the full .disp suite runs without the multi-GB heap
// the eager backend needs (and ~2× faster). Falls back to eager when rust-eager is
// absent, so a fresh checkout with no Rust toolchain still works.
export const defaultBackendName = rustEagerAvailable() ? "rust-eager" : "eager"

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
