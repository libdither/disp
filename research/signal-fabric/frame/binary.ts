// Binary add, ripple carry, on a list (addb) and on a tree (addt), for cases.mts, score.bend (through
// cases.bend) and the viewer. addb is made only of tri, not, second and a fix, so loopnet can run it.
//
// Two numbers of n bits go in as one list, lowest bit first, of positions (c, a, b): c is a carry slot,
// △ before and after, and a bit is △ (0) or △△ (1), so (2, 3) in 2 bits is
// (△, (△, (△△, (△, (△△, (△△, △)))))). addb gives back a, and a + b mod 2^n in place of b.
import { F, L, S, regs, registersOf, type V } from '../../../website/src/lib/evaluators/twoway/core.ts'

/** when the left part is 1, f runs on the right part */
const when1 = (f: string) => `tri(id, second(${f}), id)`
// at a position's node (c, (a, (b, (c', …)))), c' being the next position's carry; each when1 tests the
// bit on the left and moves one bit on
const toC1 = when1(when1('second(first(not))')) // c' ^= c·a
const toC2 = when1(`second(${when1('first(not)')})`) // c' ^= c·b
const toC3 = `second(${when1(when1('first(not)'))})` // c' ^= a·b
const sumA = `second(${when1('first(not)')})` // b ^= a
const sumC = when1('second(first(not))') // b ^= c

/** c' = majority(c, a, b) goes on, the positions above add, c' is taken back (a and b are as they
 * were), then b becomes its bit of the sum */
export const ADDB = `fix(${toC1} ; ${toC2} ; ${toC3} ; second(second(second(self))) ; ${toC3} ; ${toC2} ; ${toC1} ; ${sumA} ; ${sumC})`

// On a list every bit's right part holds every later bit, so a tri that tests a bit and changes
// something to its right reaches all the carry needs. In a tree a bit's right part is only its partner,
// so the carry has to leave a part: tri's fork case, which runs on ((l, r), x) as (l, (r, x)), is how.
//
// addt adds words <a, b> (the library's: a balanced tree, high half left, each bit pair (a, b) under a
// stem) with one carry bit beside the word, (<a, b>, c): the carry rides through the word, lowest bit
// first and back (Cuccaro, Draper, Kutin and Moulton's ripple adder, with the carry moving instead of
// wires). It needs no slots, unit, cross or copy, only tri's fork case, which loopnet does not have yet.
const tof = `tri(id, id, ${when1(when1('not'))})` // ((x, y), z): z ^= x·y
const MAJ = `swap ; ${when1('par(not, not)')} ; swap ; ${tof}` // ((a, b), c) → ((a ^ c, b ^ c), majority)
const UMA = `${tof} ; swap ; ${when1('first(not)')} ; swap ; first(${when1('not')})` // → ((a, a ^ b ^ c), c)
// at a fork, (H, (L, c)) after tri's fork case: the carry visits the low half L, or the high half H
const low = 'second(self)'
const high = 'swap ; tri(id, id, second(swap) ; second(self) ; second(swap)) ; swap'
export const ADDT = `fix(tri(id, ${MAJ}, ${low} ; ${high})) ; fix(tri(id, ${UMA}, ${high} ; ${low}))`

/** what each program does, for the viewer */
export const GLOSS: Record<string, string> = {
  addb: "addb, at each position (c, a, b), lowest first: c' ^= c·a, c' ^= c·b, c' ^= a·b (c', the next position's carry slot, gets majority(c, a, b)); the positions above; the same three again (c' back to 0); b ^= a; b ^= c",
  addt: 'addt: one carry bit rides up the word, lowest bit first (each bit pair keeps a ^ c, b ^ c and hands on majority(a, b, c)), then back down (each pair becomes a, a + b\'s bit)'
}

const bit = (b: number): V => (b ? S(L()) : L())
export function register(a: number, b: number, n: number): V {
  let t: V = L()
  for (let i = n - 1; i >= 0; i--) t = F(L(), F(bit((a >> i) & 1), F(bit((b >> i) & 1), t)))
  return t
}

const bin = (x: number, n: number) => x.toString(2).padStart(n, '0')
/** <a, b> in n bits (1, 2, 4, …) with a carry of 0 beside it, addt's input */
export const words = (a: number, b: number, n: number): V => F(regs([bin(a, n), bin(b, n)]), L())

const bitOf = (v: V): number | null => (v.tag === 'leaf' ? 0 : v.tag === 'stem' && v.c.tag === 'leaf' ? 1 : null)
/** a, b and n, if v holds a register pair with its carry slots empty */
export function readRegister(v: V): { a: number; b: number; n: number } | null {
  let a = 0, b = 0, n = 0
  for (; v.tag === 'fork'; n++) {
    if (v.l.tag !== 'leaf' || v.r.tag !== 'fork' || v.r.r.tag !== 'fork') return null
    const x = bitOf(v.r.l), y = bitOf(v.r.r.l)
    if (x === null || y === null) return null
    a |= x << n
    b |= y << n
    v = v.r.r.r
  }
  return v.tag === 'leaf' && n > 0 ? { a, b, n } : null
}

/** a, b and n, if v holds a word pair with its carry 0 */
export function readWords(v: V): { a: number; b: number; n: number } | null {
  const rs = v.tag === 'fork' && v.r.tag === 'leaf' ? registersOf(v.l) : null
  return rs?.length === 2 ? { a: parseInt(rs[0], 2), b: parseInt(rs[1], 2), n: rs[0].length } : null
}
