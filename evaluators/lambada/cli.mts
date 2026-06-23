// Selectable lambada-evaluator CLI for disp's batch tier + benchmark harness.
//
// The vendored main.mts hardcodes the lazy-stacks reducer; this generalizes it to
// ANY of the 11 evaluators in lambada-llc/tree-calculus, so each can be a
// registered batch peer (src/eval/lambada.ts) and a benchmark contestant
// (bench/bench-evaluators.ts). build.sh bundles this to artifacts/cli.cjs.
//
//   node cli.cjs -e <evaluator> -ternary <term> <term> ...
//
// Folds the ternary terms by application from identity (≡ from term0, since
// apply(id,t0)=t0) and prints the ternary normal form on stdout; prints
// `reduce_ms=<n> evaluator=<name>` on stderr — startup-free reduction time for the
// harness. stdout stays a clean NF, so this is also a valid BatchRunner.

import { Evaluator, id, raise } from "./vendor/implementation/typescript/src/common.mjs"
import ternary from "./vendor/implementation/typescript/src/format/ternary.mjs"

import eager_func from "./vendor/implementation/typescript/src/evaluator/eager-func.mjs"
import eager_node_app from "./vendor/implementation/typescript/src/evaluator/eager-node-app.mjs"
import eager_stacks from "./vendor/implementation/typescript/src/evaluator/eager-stacks.mjs"
import eager_value_adt from "./vendor/implementation/typescript/src/evaluator/eager-value-adt.mjs"
import eager_value_memory from "./vendor/implementation/typescript/src/evaluator/eager-value-memory.mjs"
import eager_value_memoizing from "./vendor/implementation/typescript/src/evaluator/eager-value-memoizing.mjs"
import eager_value_memoizing_alt from "./vendor/implementation/typescript/src/evaluator/eager-value-memoizing-alt.mjs"
import lazy_func from "./vendor/implementation/typescript/src/evaluator/lazy-func.mjs"
import lazy_stacks from "./vendor/implementation/typescript/src/evaluator/lazy-stacks.mjs"
import lazy_value_adt from "./vendor/implementation/typescript/src/evaluator/lazy-value-adt.mjs"
import lazy_value_memory from "./vendor/implementation/typescript/src/evaluator/lazy-value-memory.mjs"

// name -> factory (fresh Evaluator). Direct-export evaluators are wrapped; the
// four make_evaluator ones are factories already (fresh memo/arena ctx per call).
const REGISTRY: Record<string, () => Evaluator<unknown>> = {
  "eager-func": () => eager_func as Evaluator<unknown>,
  "eager-node-app": () => eager_node_app as Evaluator<unknown>,
  "eager-stacks": () => eager_stacks as Evaluator<unknown>,
  "eager-value-adt": () => eager_value_adt as Evaluator<unknown>,
  "eager-value-memory": eager_value_memory as () => Evaluator<unknown>,
  "eager-value-memoizing": eager_value_memoizing as () => Evaluator<unknown>,
  "eager-value-memoizing-alt": eager_value_memoizing_alt as () => Evaluator<unknown>,
  "lazy-func": () => lazy_func as Evaluator<unknown>,
  "lazy-stacks": () => lazy_stacks as Evaluator<unknown>,
  "lazy-value-adt": () => lazy_value_adt as Evaluator<unknown>,
  "lazy-value-memory": lazy_value_memory as () => Evaluator<unknown>,
}

export const EVALUATORS: string[] = Object.keys(REGISTRY)

export function fold(evname: string, terms: string[]): string {
  const make = REGISTRY[evname] ?? raise(`unknown evaluator '${evname}' (have: ${EVALUATORS.join(", ")})`)
  const e = make()
  let acc = id(e)
  for (const t of terms) acc = e.apply(acc, ternary.of(e, t))
  return ternary.to(e, acc)
}

if (typeof require !== "undefined" && require.main === module) {
  const argv = process.argv.slice(2)
  let evname = "lazy-stacks"
  const terms: string[] = []
  for (let i = 0; i < argv.length; i++) {
    const a = argv[i]
    if (a === "-e") evname = argv[++i]
    else if (a === "-ternary") continue
    else terms.push(a)
  }
  if (terms.length === 0) raise("no ternary terms given")
  const t0 = performance.now()
  const out = fold(evname, terms)
  const ms = performance.now() - t0
  process.stdout.write(out + "\n")
  process.stderr.write(`reduce_ms=${ms.toFixed(3)} evaluator=${evname}\n`)
}
