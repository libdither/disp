// Minimal posix path shim for the in-browser disp compiler.
// The elaborator driver only needs dirname/resolve/join over the virtual
// filesystem's rooted paths ('/lib/...'), so a strict posix subset suffices.

export function normalize(p: string): string {
  const abs = p.startsWith('/')
  const out: string[] = []
  for (const seg of p.split('/')) {
    if (seg === '' || seg === '.') continue
    if (seg === '..') {
      if (out.length && out[out.length - 1] !== '..') out.pop()
      else if (!abs) out.push('..')
      continue
    }
    out.push(seg)
  }
  const body = out.join('/')
  return abs ? '/' + body : body || '.'
}

export function resolve(...parts: string[]): string {
  let acc = '/'
  for (const p of parts) {
    if (!p) continue
    acc = p.startsWith('/') ? p : acc + '/' + p
  }
  return normalize(acc)
}

export function dirname(p: string): string {
  const n = normalize(p)
  const i = n.lastIndexOf('/')
  if (i < 0) return '.'
  if (i === 0) return '/'
  return n.slice(0, i)
}

export function join(...parts: string[]): string {
  return normalize(parts.filter(Boolean).join('/'))
}

export function basename(p: string): string {
  const n = normalize(p)
  const i = n.lastIndexOf('/')
  return i < 0 ? n : n.slice(i + 1)
}

export default { normalize, resolve, dirname, join, basename }
