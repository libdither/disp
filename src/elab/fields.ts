// Record fields picked by type. A `record` declaration binds each accessor under
// its qualified name `Type.field` (SYNTAX.typ § record), so two records may share
// a field name. `x.field` and a bare `field` resolve here: by the receiver's `::`
// type when it is known, by the only accessor in scope when there is one, and
// otherwise as an error asking for an annotation. Types come from annotations,
// which are never checked, so a type only ever breaks a tie.
import { isListLiteral, type Expr } from "../parse.js"
import { elab, type ScopeEntry } from "./state.js"

/// field name → record type name → the field's declared type
const owners = new Map<string, Map<string, Expr | null>>()

export const qualified = (type: string, field: string): string => `${type}.${field}`

export function registerField(type: string, field: string, fieldType: Expr | null): void {
  let byType = owners.get(field)
  if (!byType) owners.set(field, byType = new Map())
  if (!byType.has(type) || fieldType != null) byType.set(type, fieldType)
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

/// Unwrap `Implicit({A} => ...)` / `Forall({A} => ...)` / `{A : Type} -> ...` quantification
/// around a type expression so `typeName` and `typeOf` see the underlying return/function shape.
export function unwrapTypeExpr(ty: Expr | null | undefined): Expr | null {
  let cur = ty ?? null
  while (cur != null) {
    if (cur.tag === "app" && cur.f.tag === "var" && (cur.f.name === "Implicit" || cur.f.name === "Forall")
        && cur.x.tag === "binder" && cur.x.fat) {
      cur = cur.x.body
      continue
    }
    if (cur.tag === "binder" && !cur.fat && cur.params.length > 0) {
      const p0 = cur.params[0]
      if (p0.type?.tag === "var" && (p0.type.name === "Type" || p0.type.name === "TypeFam")) {
        cur = cur.params.length === 1 ? cur.body : { ...cur, params: cur.params.slice(1) }
        continue
      }
    }
    break
  }
  return cur
}

/// Peel one function argument from a type expression (`A -> B` or `PartialFn(A, B)`).
function peelOneArg(ty: Expr | null | undefined): Expr | null {
  const fn = unwrapTypeExpr(ty)
  if (fn == null) return null
  if (fn.tag === "binder" && !fn.fat && fn.params.length > 0)
    return fn.params.length === 1 ? fn.body : { ...fn, params: fn.params.slice(1) }
  if (fn.tag === "app" && fn.f.tag === "app" && fn.f.f.tag === "var" && fn.f.f.name === "PartialFn")
    return fn.x
  return null
}

/// Strip the leading receiver parameter from a method's declared type (`Option(A) -> (A -> B) -> Option(B)`
/// → `(A -> B) -> Option(B)`), so chained dot calls (`x.map(f).unwrap_or(d)`) keep knowing the return type.
export function peelReceiverType(ty: Expr | null | undefined): Expr | null {
  return peelOneArg(ty)
}

/// a type's head constructor name (`Former`, `List(Nat)` → `"List"`, `Option(A)` → `"Option"`)
export const typeName = (ty: Expr | null | undefined): string | null => {
  let cur = unwrapTypeExpr(ty)
  while (cur != null && cur.tag === "app") cur = cur.f
  return cur != null && cur.tag === "var" ? cur.name : null
}

/// The declared type of an expression, as far as annotations tell: a local or top-level
/// annotation, a numeric literal (`Nat`), a field/method's declared type, an applied
/// function type's result, or an ascription.
export function typeOf(e: Expr, lookupEntry: (name: string) => ScopeEntry | undefined): Expr | null {
  if (isListLiteral(e)) return { tag: "var", name: "List" }
  switch (e.tag) {
    case "var": return localType(e.name) ?? lookupEntry(e.name)?.typeExpr ?? null
    case "num": return { tag: "var", name: "Nat" }
    case "ann": return e.type
    case "proj": {
      if (e.target.tag === "var" && !isLocal(e.target.name) && lookupEntry(e.target.name)?.fields == null) {
        const direct = lookupEntry(qualified(e.target.name, e.field))
        if (direct?.typeExpr) return direct.typeExpr
      }
      const type = typeName(typeOf(e.target, lookupEntry))
      const owner = pickOwner(type, e.field, lookupEntry, false)
      return owner ? fieldTypeOf(owner, e.field) : null
    }
    case "app": return peelOneArg(typeOf(e.f, lookupEntry))
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
  const fnTree = lookupEntry(field)?.tree
  const fn = fnTree != null
  if (found.length === 1 && (!fn || elab.cs.equal!(lookupEntry(qualified(found[0], field))!.tree!, fnTree!))) return found[0]
  if (type != null && fn) return null
  if (!strict) return null
  const choices = found.map(t => `'${t}'`).join(", ")
  if (bare) throw new Error(`'${field}' is a field of ${choices}; write ${found.map(t => qualified(t, field)).join(" or ")}`)
  throw new Error(type == null
    ? `'.${field}' is ambiguous: ${fn ? `the function '${field}' or ` : ""}a field of ${choices}; annotate the receiver's type`
    : `'.${field}' on a '${type}': ${type} has no field '${field}', and it is a field of ${choices}`)
}
