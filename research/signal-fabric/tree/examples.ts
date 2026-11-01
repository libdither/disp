// The programs the player offers, each with an input value (core.ts's text syntax).
import { asBit, bit, F, S, regs, registersOf, type V } from '../../../website/src/lib/evaluators/twoway/core.ts'
export interface Example { name: string; prog: string; input: string; tip: string }
export const EXAMPLES: Example[] = [
  { name: 'swap', prog: 'swap', input: '(△△, (△, △))', tip: 'One move at the top node: its two wires trade ports. Nothing else moves.' },
  { name: 'cross', prog: 'cross', input: '((△, △△), △△△)', tip: 'One move, two nodes: two wires change the node they hang from, spliced along the wire between them.' },
  { name: 'mirror', prog: 'mirror', input: '(((T, F), (F, T)), ((T, T), (F, F)))', tip: 'map sends a signal down both wires of every fork at once; each fork swaps when its halves are done.' },
  { name: 'not', prog: 'not', input: 'T', tip: 'Seven moves on one bit: a bit is a tiny tree, △ or △△, and not is a dance of moves around it.' },
  { name: 'notw', prog: 'notw', input: '<0b10110010>', tip: 'The program spreads down the word in parallel, flips each bit where it is, and the done signals gather back up.' },
  { name: 'cnotw', prog: 'cnotw', input: '<0b10110010, 0b01101100>', tip: 'b becomes b xor a at every bit at once.' },
  { name: 'addw 4', prog: 'addw', input: '<0b1011, 0b0110>', tip: 'Statuses go up the word tree (up), carries come down it (down): a carry-lookahead adder made of the program itself.' },
  { name: 'addw 8', prog: 'addw', input: '<0b10110010, 0b01101100>', tip: 'The same adder on 8-bit words: one more level of the tree, not twice the time.' },
  { name: 'transpose', prog: 'transpose', input: '((△, △△), (△△△, △△△△))', tip: '((a, b), (c, d)) → ((a, c), (b, d)): rot, xch and rot† rewire four subtrees; the wires splice and unsplice.' },
  { name: 'copy', prog: 'copy', input: '(△△, (△, △))', tip: 'Copying is a program: dup grows a copy of x, node by node, out of new nodes.' }
]

/** The same shape with other bits: new words for registers, else each bit in the tree drawn again. */
export function otherBits(v: V, rand = Math.random): V {
  const r = asBit(v) === null && registersOf(v)
  if (r) return regs(r.map((w) => [...w].map(() => (rand() < 0.5 ? '1' : '0')).join('')))
  const go = (t: V): V => (asBit(t) !== null ? bit(rand() < 0.5) : t.tag === 'fork' ? F(go(t.l), go(t.r)) : t.tag === 'stem' ? S(go(t.c)) : t)
  return go(v)
}
