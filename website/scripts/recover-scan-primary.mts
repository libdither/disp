// Provenance of XMR_SCAN_PRIMARY in build-dono-stats.mts — run once, kept for
// audit. Reconstructs a scanning-equivalent Monero PRIMARY address from
// (private view key, subaddress): subaddress spend key D = B +
// Hs("SubAddr\0"||a||idx)*G, so B' = D - m(0,1)*G defines a wallet whose
// subaddress (0,1) IS the given subaddress; view-only detection
// (P - Hs(aR||i)G in the table) then finds exactly the outputs paid to it.
// Only donations to the published subaddress are visible — deliberately
// narrower than the wallet's real primary would reveal. The stats script
// re-asserts the derivation against monero-ts on every run.
//
// Needs @noble ad hoc (not a project dep): npm i --no-save @noble/curves @noble/hashes
// then: npx tsx scripts/recover-scan-primary.mts
import { ed25519 } from '@noble/curves/ed25519.js'
import { keccak_256 } from '@noble/hashes/sha3.js'

const SUBADDR = '87ErQFwijiDCqcPs1yy8qGdaLBA2BG1BqAy93aE4pGYe2KFbZuxJDiGjjgLtDrzgxFaK98YHv7rctYfyCfptAvmHDWN8MGv'
const VIEW_KEY_HEX = 'e6cf097d491f78f5a8443436c43b54985a0c5a0702e683f2515a57ff29942306'

const ALPHA = '123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz'
const ENC_SIZES = [0, 2, 3, 5, 6, 7, 9, 10, 11]
function b58decode(s: string): Uint8Array {
  const out: number[] = []
  for (let i = 0; i < s.length; i += 11) {
    const block = s.slice(i, i + 11)
    const bytes = ENC_SIZES.indexOf(block.length)
    if (bytes < 0) throw new Error('bad block length')
    let n = 0n
    for (const c of block) {
      const d = ALPHA.indexOf(c)
      if (d < 0) throw new Error('bad char')
      n = n * 58n + BigInt(d)
    }
    const arr = new Uint8Array(bytes)
    for (let j = bytes - 1; j >= 0; j--) {
      arr[j] = Number(n & 0xffn)
      n >>= 8n
    }
    out.push(...arr)
  }
  return new Uint8Array(out)
}
function b58encode(data: Uint8Array): string {
  let out = ''
  for (let i = 0; i < data.length; i += 8) {
    const block = data.slice(i, i + 8)
    let n = 0n
    for (const b of block) n = (n << 8n) | BigInt(b)
    const chars = ENC_SIZES[block.length] // i bytes -> ENC_SIZES[i] chars
    let s = ''
    for (let j = 0; j < chars; j++) {
      s = ALPHA[Number(n % 58n)] + s
      n /= 58n
    }
    out += s
  }
  return out
}

const hex = (u: Uint8Array) => [...u].map((b) => b.toString(16).padStart(2, '0')).join('')
const unhex = (s: string) => new Uint8Array(s.match(/../g)!.map((h) => parseInt(h, 16)))
const L = ed25519.Point.Fn.ORDER
const leToBig = (u: Uint8Array) => {
  let n = 0n
  for (let i = u.length - 1; i >= 0; i--) n = (n << 8n) | BigInt(u[i])
  return n
}

const decoded = b58decode(SUBADDR)
if (decoded[0] !== 0x2a) throw new Error(`not a mainnet subaddress (tag 0x${decoded[0].toString(16)})`)
const D = ed25519.Point.fromHex(hex(decoded.slice(1, 33)))
const Csub = decoded.slice(33, 65)
const a = leToBig(unhex(VIEW_KEY_HEX)) % L
const A = ed25519.Point.BASE.multiply(a)

// the pair check: a subaddress's view pub is a*D — this proves the view key
// genuinely belongs to this subaddress before we build anything on it
if (hex(D.multiply(a).toBytes()) !== hex(Csub)) throw new Error('view key does NOT belong to this subaddress')
console.log('view key ✓ (C = a·D holds)')

const pre = new TextEncoder().encode('SubAddr\0')
const idx = new Uint8Array(8)
new DataView(idx.buffer).setUint32(0, 0, true) // major 0
new DataView(idx.buffer).setUint32(4, 1, true) // minor 1
const m = leToBig(keccak_256(new Uint8Array([...pre, ...unhex(VIEW_KEY_HEX), ...idx]))) % L
const B = D.subtract(ed25519.Point.BASE.multiply(m))

const payload = new Uint8Array([0x12, ...B.toBytes(), ...A.toBytes()])
const ck = keccak_256(payload).slice(0, 4)
console.log('scanning primary (subaddress sits at index (0,1) of this wallet):')
console.log(b58encode(new Uint8Array([...payload, ...ck])))