// Backend registry: name -> EvalBackend, for `--evaluator=<name>` resolution.
// The eager reference and naive conformance backends are always registered;
// built foreign backends register conditionally (see EVALUATOR.md).

import type { EvalBackend } from "./types.js"
import { eagerBackend } from "./eager.js"
import { naiveBackend } from "./naive.js"
import { rustEagerBackend, rustEagerAvailable } from "./rust-eager.js"
import { rustEagerNativeBackend, rustEagerNativeAvailable } from "./rust-eager-native.js"
import { icNetBackend, icNetAvailable } from "./ic-net.js"

// Handles are opaque to the engine, so backends are stored at `unknown` handle
// type; each backend casts internally (the eager/naive backends' H = Tree).
const backends = new Map<string, EvalBackend>([
  [eagerBackend.name, eagerBackend as unknown as EvalBackend],
  [naiveBackend.name, naiveBackend as unknown as EvalBackend],
])

// Foreign in-process backends register only if their build artifact exists, so
// the everyday loop needs no foreign toolchain (EVALUATOR.md § Layout). rust-eager is
// a Rust→WASM Session backend; absent until evaluators/rust-eager/build.sh runs.
if (rustEagerAvailable()) backends.set(rustEagerBackend.name, rustEagerBackend as unknown as EvalBackend)

// rust-eager-native: the SAME hash-consed eager reducer as an in-process N-API addon (not
// wasm), so the Session runs in host RAM instead of the wasm32 4 GiB ceiling. Registers when
// evaluators/rust-eager/build-native.sh has produced the .node.
if (rustEagerNativeAvailable()) backends.set(rustEagerNativeBackend.name, rustEagerNativeBackend as unknown as EvalBackend)

// rust-ic-net: the materialized interaction-net backend (M0). Registers when built,
// but is NEVER the default — it's a batch/differential + optimizer substrate, not the
// checker (no hash-consing ⇒ no O(1) conversion). See RUST_IC_NET_DESIGN.md.
if (icNetAvailable()) backends.set(icNetBackend.name, icNetBackend as unknown as EvalBackend)

// Default to the fastest / most-capable backend whose artifact is built. Preference order:
//   rust-eager-native (in-process N-API)  — host RAM, no wasm32 4 GiB ceiling, ~1.4× the
//                                            wasm backend; passes kernel modules the wasm
//                                            arena OOMs on (e.g. wrapping.test.disp in
//                                            shared-session mode). Built by build-native.sh.
//   rust-eager (Rust→WASM)                — linear memory off the V8 heap, ~2× the TS eager.
//   eager (TS reference)                  — always present; needs the multi-GB V8 heap.
// Each foreign backend is gated on its artifact existing (an explicit build), so a fresh
// checkout with no Rust toolchain still falls all the way back to eager.
export const defaultBackendName = rustEagerNativeAvailable()
  ? "rust-eager-native"
  : rustEagerAvailable()
    ? "rust-eager"
    : "eager"

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
