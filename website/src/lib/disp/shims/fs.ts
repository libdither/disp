// node:fs shim for the in-browser disp compiler: a read-only virtual
// filesystem preloaded with the entire disp library (lib/**/*.disp), bundled
// as raw strings at build time. The elaborator's module loader (modscan.ts)
// only ever calls readFileSync on `use`d module paths, which resolve to
// '/lib/...' under the virtual root (the playground buffer itself is passed
// to parseProgram as source text, never read from disk).

// (__site_example_* are transient files validate-examples.mts writes into
// lib/tests/ while it runs — never part of the library, and letting them into
// the vfs would skew the kernel-snapshot lib hash whenever validation overlaps
// a dev session or build.)
const modules = import.meta.glob(['../../../../../lib/**/*.disp', '!**/__site_example_*'], {
  query: '?raw',
  import: 'default',
  eager: true
}) as Record<string, string>

export const vfs = new Map<string, string>()
for (const [key, text] of Object.entries(modules)) {
  // '../../../../../lib/kernel/prelude.disp' -> '/lib/kernel/prelude.disp'
  const rooted = key.replace(/^(\.\.\/)+/, '/')
  vfs.set(rooted, text)
}

export function readFileSync(path: string, _enc?: unknown): string {
  const p = path.startsWith('/') ? path : '/' + path
  const hit = vfs.get(p)
  if (hit !== undefined) return hit
  throw new Error(`ENOENT: no such file in the playground's virtual filesystem: ${p}`)
}

export function existsSync(path: string): boolean {
  const p = path.startsWith('/') ? path : '/' + path
  return vfs.has(p)
}

export default { readFileSync, existsSync }
