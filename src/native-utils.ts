// Shared utilities for the native type system.
//
// Extracted from coc.ts so that both the CoC pipeline and the tree-native
// pipeline can share marker generation and native-builtin tracking without
// creating a circular dependency.

import { type Tree, LEAF, stem, fork } from "./tree.js"

// ============================================================
// Fresh markers
// ============================================================

let markerCounter = 0

export function freshMarker(): Tree {
  let t: Tree = LEAF
  for (let i = 0; i < markerCounter; i++) {
    t = stem(t)
  }
  markerCounter++
  return fork(LEAF, t)
}

export function resetMarkerCounter(): void {
  markerCounter = 0
}

// ============================================================
// Native builtin tracking
// ============================================================
//
// Native combinator trees (from compileTree / bracket abstraction) can
// accidentally match CoC encoding patterns (e.g., fork(LEAF,...) = encApp).
// We track their IDs so whnfTree / evalToNative can avoid decomposing them.
//
// For some trees (like recursive definitions), the encoded form (omega
// combinator) differs from the compiled form (FIX-based). We map encoded
// IDs to their compiled forms so evalToNative uses the version that works
// with eager apply().

const nativeBuiltinIds = new Set<number>()
const nativeCompiledForms = new Map<number, Tree>()

export function registerNativeBuiltinId(id: number, compiledForm?: Tree): void {
  nativeBuiltinIds.add(id)
  if (compiledForm) {
    nativeCompiledForms.set(id, compiledForm)
    nativeBuiltinIds.add(compiledForm.id)
  }
}

// Callbacks invoked when native builtins are cleared (e.g., to invalidate caches)
const clearCallbacks: (() => void)[] = []

export function onClearNativeBuiltins(cb: () => void): void {
  clearCallbacks.push(cb)
}

export function clearNativeBuiltins(): void {
  nativeBuiltinIds.clear()
  nativeCompiledForms.clear()
  for (const cb of clearCallbacks) cb()
}

export function isNativeBuiltin(t: Tree): boolean {
  return nativeBuiltinIds.has(t.id)
}

export function getNativeForm(t: Tree): Tree {
  return nativeCompiledForms.get(t.id) ?? t
}
