// The programs the player offers, each with an input that fixes its shape: registers or a tree of bits.
export interface Example { name: string; prog: string; input: string; tip: string }
export const EXAMPLES: Example[] = [
  { name: 'not', prog: 'not', input: 'T', tip: 'One bit flipped: a single gate.' },
  { name: 'cnot', prog: 'cnot', input: '(T, F)', tip: 'The right bit flips when the left is T: one xor gate, the left bit wired through.' },
  { name: 'toffoli', prog: 'toffoli', input: '((T, T), F)', tip: 'Two controls: the target flips when both are T.' },
  { name: 'notw', prog: 'notw', input: '<0b10110010>', tip: 'Every bit of a word flipped at once: a column of inverters.' },
  { name: 'cnotw', prog: 'cnotw', input: '<0b10110010, 0b01101100>', tip: 'b becomes b xor a, bit by bit: a column of xor gates.' },
  { name: 'mirror', prog: 'mirror', input: '(((T, F), (F, T)), ((T, T), (F, F)))', tip: 'Swaps the halves of every fork: no gates at all, only wires crossing.' },
  { name: 'addw 4', prog: 'addw', input: '<0b1011, 0b0110>', tip: '<a, b> → <a, a + b>: a ripple adder, the carry running from the low bits (right) to the high.' },
  { name: 'addw 8', prog: 'addw', input: '<0b10110010, 0b01101100>', tip: 'The same adder on 8-bit words: twice the carry chain.' },
  { name: 'sub 4', prog: 'addw†', input: '<0b1011, 0b0001>', tip: 'addw flipped: <a, s> → <a, s − a>. A different circuit, compiled from the same program run backward.' }
]
