// A buffer travels in the URL hash: 'c' + base64url(deflate-raw bytes) where
// CompressionStream exists, else 'r' + base64url(utf-8 bytes). The playground
// decodes it on mount; the landing card encodes an edited buffer into its
// "open in playground" link so the edit follows the visitor.

function b64url(b: Uint8Array): string {
  let s = ''
  for (const x of b) s += String.fromCharCode(x)
  return btoa(s).replaceAll('+', '-').replaceAll('/', '_').replace(/=+$/, '')
}

/// Encodes a document into the hash payload (without the leading '#').
export async function encodeShared(doc: string): Promise<string> {
  const bytes = new TextEncoder().encode(doc)
  if (typeof CompressionStream === 'undefined') return 'r' + b64url(bytes)
  const stream = new Blob([bytes]).stream().pipeThrough(new CompressionStream('deflate-raw'))
  return 'c' + b64url(new Uint8Array(await new Response(stream).arrayBuffer()))
}

/// Decodes a hash payload (without '#'); null when it is not a share payload.
export async function decodeShared(h: string): Promise<string | null> {
  try {
    const raw = Uint8Array.from(atob(h.slice(1).replaceAll('-', '+').replaceAll('_', '/')), (c) =>
      c.charCodeAt(0)
    )
    if (h[0] === 'r') return new TextDecoder().decode(raw)
    if (h[0] !== 'c' || typeof DecompressionStream === 'undefined') return null
    const stream = new Blob([raw]).stream().pipeThrough(new DecompressionStream('deflate-raw'))
    return await new Response(stream).text()
  } catch {
    return null
  }
}

/// True for a hash that carries a shared buffer.
export const isSharedHash = (h: string): boolean => h.length > 1 && (h[0] === 'c' || h[0] === 'r')
