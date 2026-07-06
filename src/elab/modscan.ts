// Parsed-items cache + the syntactic `given` pre-scan (MODULES.md § Surface).
// File content is immutable per process, so parses are cached host-globally.
// Givens are discovered syntactically (head = the identifier `given`) so fill
// resolution can validate and key an instantiation BEFORE elaborating the file;
// a custom (shadowed) `given` decorator that produces a param-request outside
// this scan is rejected by the driver (dynamic givens are not supported).

import { readFileSync } from "node:fs"
import { parseItems, type Expr, type RecMember } from "../parse.js"

const parsedItemsCache = new Map<string, RecMember[]>()
export function parseFileItems(abs: string): RecMember[] {
  let items = parsedItemsCache.get(abs)
  if (!items) {
    items = parseItems(readFileSync(abs, "utf-8"))
    parsedItemsCache.set(abs, items)
  }
  return items
}

export const isGivenHead = (head: Expr | undefined): boolean =>
  head?.tag === "var" && head.name === "given"

// A module dependency: `given name : type (:= dflt)?`. Fills are explicit
// everywhere; under `use raw` an unfilled, undefaulted given is simply NOT IN
// SCOPE (nothing implicit flows — a value referencing it errors as unbound,
// naming the file). The kernel bootstrap rides this: fragment givens are
// annotation-only, and raw drops annotations.
export type GivenSpec = { name: string; type: Expr | null; dflt: Expr | null }

export function scanGivens(items: RecMember[]): GivenSpec[] {
  const out: GivenSpec[] = []
  for (const it of items)
    if (it.tag === "field" && isGivenHead(it.head))
      out.push({ name: it.name, type: it.type, dflt: it.value ?? null })
  return out
}
