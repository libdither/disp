// The batch tier (EVALUATOR_PLAN.md §4.5): a one-shot interface that takes
// ternary terms, left-folds application, and returns the ternary normal form —
// the de-facto lambada benchmark interchange contract. Any in-process Session is
// a contestant via sessionBatchRunner; external standalone CLIs implement the
// same shape over stdio (future — needs their binaries; see bench/evaluators.json
// for the declaration format). Used for benchmarks and differential conformance
// only, never for elaboration.

import type { Session, Budget } from "./types.js"

export interface BatchRunner {
  readonly name: string
  // Left-fold apply over the terms, returning the ternary NF. (lambada folds
  // "from identity"; since apply(I, t0) ≡ t0, starting from t0 is equivalent and
  // needs no I handle. Reconciling the exact ternary byte convention with
  // lambada's conventions/ is part of wiring real peers.)
  fold(terms: string[], budget?: Budget): string
}

export function sessionBatchRunner<H>(name: string, session: Session<H>): BatchRunner {
  return {
    name,
    fold(terms, budget) {
      if (terms.length === 0) throw new Error("batch fold: no terms")
      let acc: H = session.loadTernary(terms[0])
      for (let i = 1; i < terms.length; i++) {
        acc = session.apply(acc, session.loadTernary(terms[i]), budget)
      }
      return session.dumpTernary(acc, budget)
    },
  }
}
