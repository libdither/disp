// Record fields picked by type. A `record` declaration binds each accessor under
// its qualified name `Type.field` (SYNTAX.typ § record), so two records may share
// a field name. `x.field` and a bare `field` resolve here: by the receiver's `::`
// type when it is known, by the only accessor in scope when there is one, and
// otherwise as an error asking for an annotation. Types come from annotations,
// which are never checked, so a type only ever breaks a tie.
import type { Expr } from "../parse.js"
import type { ScopeEntry } from "./state.js"

/// field name → record type name → the field's declared type
const owners = new Map<string, Map<string, Expr | null>>()

export const qualified = (type: string, field: string): string => `${type}.${field}`

export function registerField(type: string, field: string, fieldType: Expr | null): void {
  let byType = owners.get(field)
  if (!byType) owners.set(field, byType = new Map())
  byType.set(type, fieldType)
}

/// the declared type of a qualified accessor, when one is registered
export function fieldTypeOf(type: string, field: string): Expr | null {
  return owners.get(field)?.get(type) ?? null
}

/// record types with an accessor for `field` in this scope
export function ownersInScope(field: string, lookupEntry: (name: string) => ScopeEntry | undefined): string[] {
  const byType = owners.get(field)
  if (!byType) return []
  return [...byType.keys()].filter(type => lookupEntry(qualified(type, field))?.tree != null)
}

// Local binders, innermost last: name → its annotated type, or null when unannotated.
const frames: Map<string, Expr | null>[] = []

export function withLocals<T>(locals: Map<string, Expr | null>, body: () => T): T {
  frames.push(locals)
  try { return body() } finally { frames.pop() }
}

/// undefined when the name is not a local binder
export function localType(name: string): Expr | null | undefined {
  for (let i = frames.length - 1; i >= 0; i--)
    if (frames[i].has(name)) return frames[i].get(name)
  return undefined
}

export const isLocal = (name: string): boolean => localType(name) !== undefined

/// a type's name, when it is a plain identifier (`Former`, not `List(Former)`)
export const typeName = (ty: Expr | null | undefined): string | null =>
  ty != null && ty.tag === "var" ? ty.name : null

/// The `::` type of an expression, as far as annotations tell: a local's annotation,
/// a field's declared type, an applied function type's result, or an ascription.
export function typeOf(e: Expr, lookupEntry: (name: string) => ScopeEntry | undefined): Expr | null {
  switch (e.tag) {
    case "var": return localType(e.name) ?? null
    case "ann": return e.type
    case "proj": {
      const type = typeName(typeOf(e.target, lookupEntry))
      const owner = pickOwner(type, e.field, lookupEntry, false)
      return owner ? fieldTypeOf(owner, e.field) : null
    }
    case "app": {
      const fn = typeOf(e.f, lookupEntry)
      if (fn == null || fn.tag !== "binder" || fn.fat || fn.params.length === 0) return null
      return fn.params.length === 1 ? fn.body : { ...fn, params: fn.params.slice(1) }
    }
    default: return null
  }
}

/// The record whose accessor `field` reads a receiver of type `type`, or null when
/// no record in scope has the field (or a function of that name should win). With
/// `strict`, an unresolvable tie is an error instead of null; `bare` is a field
/// named on its own, with no receiver.
export function pickOwner(
  type: string | null, field: string,
  lookupEntry: (name: string) => ScopeEntry | undefined, strict = true, bare = false,
): string | null {
  const found = ownersInScope(field, lookupEntry)
  if (found.length === 0) return null
  if (type != null && found.includes(type)) return type
  const fn = lookupEntry(field)?.tree != null
  if (found.length === 1 && !fn) return found[0]
  if (type != null && fn) return null
  if (!strict) return null
  const choices = found.map(t => `'${t}'`).join(", ")
  if (bare) throw new Error(`'${field}' is a field of ${choices}; write ${found.map(t => qualified(t, field)).join(" or ")}`)
  throw new Error(type == null
    ? `'.${field}' is ambiguous: ${fn ? `the function '${field}' or ` : ""}a field of ${choices}; annotate the receiver's type`
    : `'.${field}' on a '${type}': ${type} has no field '${field}', and it is a field of ${choices}`)
}
