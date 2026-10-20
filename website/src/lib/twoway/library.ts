// The library: every program here is put together from the moves and the combinators, nothing else.
// A part marked `part` is only meant to run inside another (its self is that program's).

import { define, parseProg, type Reads } from './text'
import type { V } from './core'

export interface Def {
  name: string
  src: string
  /** What it does, in a line. */
  doc: string
  /** A piece of a bigger program, not run on its own. */
  part?: boolean
  /** Where it belongs in the table. */
  group: 'bits' | 'trees' | 'words' | 'adder'
}

export const LIBRARY: Def[] = [
  {
    group: 'bits',
    name: 'not',
    src: 'unit ; swap ; tri(stem, id, id) ; swap ; tri(stem, id, id)† ; swap ; unit†',
    doc: 'F ⇄ T: an empty slot is set where the bit is F, the bit is cleared where the slot stayed F, and dropped'
  },
  { group: 'bits', name: 'cnot', src: 'when(not)', doc: '(T, t) → (T, not t): when the left is T, not runs on the right' },
  { group: 'bits', name: 'toffoli', src: 'tri(id, id, when(cnot))', doc: '((T, T), t) → ((T, T), not t)' },
  { group: 'trees', name: 'xch', src: 'rot† ; first(swap) ; rot', doc: '(u, (v, w)) ⇄ (v, (u, w))' },
  { group: 'trees', name: 'transpose', src: 'rot ; second(xch) ; rot†', doc: '((a, b), (c, d)) ⇄ ((a, c), (b, d))' },
  { group: 'trees', name: 'cross', src: 'rot ; second(swap) ; rot†', doc: '((u, v), w) ⇄ ((u, w), v)' },
  { group: 'trees', name: 'add', src: 'fix(tri(id, self ; second(stem), id))', doc: '(a, b) → (a, a + b), numbers counted in stems' },
  {
    group: 'trees',
    name: 'dup',
    src: 'fix(tri(id, self ; second(stem), second(second(unit)) ; rot† ; transpose ; par(self, self) ; transpose ; rot))',
    doc: '(x, △) → (x, x): fills an empty place with a copy of x'
  },
  { group: 'trees', name: 'copy', src: 'unit ; swap ; dup', doc: 'x → (x, x)' },
  { group: 'trees', name: 'mirror', src: 'fix(map(self, self, self) ; swap)', doc: 'swaps the halves of every fork' },
  { group: 'words', name: 'notw', src: 'fix(map(not, self, self))', doc: 'flips every bit of a word, all at once' },
  { group: 'words', name: 'cnotw', src: 'fix(map(cnot, self, self))', doc: '<a, b> → <a, b xor a>, bit by bit' },
  { group: 'adder', name: 'prep', src: 'fix(map(unit, self, self))', doc: 'gives each bit pair (a, b) an empty carry: (△, (a, b))' },
  { group: 'adder', name: 'negb', src: 'fix(map(second(second(not)), self, self))', doc: 'flips every b' },
  { group: 'adder', name: 'sumbit', src: 'second(cnot) ; xch ; second(cnot) ; xch', doc: '(c, (a, b)) → (c, (a, b xor a xor c))', part: true },
  { group: 'adder', name: 'sumz', src: 'fix(map(sumbit, self, self))', doc: 'every b becomes its bit of the sum' },
  { group: 'adder', name: 'flat', src: 'rot ; second(rot)', doc: '((c, (a, b)), s) ⇄ (c, (a, (b, s)))', part: true },
  {
    group: 'adder',
    name: 'leafs',
    src: 'flat ; second(second(when(stem)) ; xch ; second(when(stem)) ; xch) ; flat†',
    doc: "a bit pair's status s = a + b: 0 kills a carry, 1 passes it on, 2 makes one",
    part: true
  },
  { group: 'adder', name: 'pick', src: 'cross ; first(dup) ; cross', doc: '((s, x), △) → ((s, x), s)', part: true },
  {
    group: 'adder',
    name: 'nodes',
    src: 'rot ; tri(id, tri(second(pick), second(second(second(stem ; stem))), id), id) ; rot†',
    doc: "a half-pair's status: the high half's, unless it passes, then the low half's",
    part: true
  },
  {
    group: 'adder',
    name: 'ann',
    src: 'unit ; swap ; tri(id, leafs, nodes) ; swap',
    doc: 'x → (status, x), once the parts of x have theirs',
    part: true
  },
  { group: 'adder', name: 'up', src: 'fix(map(id, self, self) ; ann)', doc: 'every part x of the word becomes (status, x), from the bits up' },
  {
    group: 'adder',
    name: 'co',
    src: 'tri(id, tri(first(cnot), second(first(second(not))), id), id)',
    doc: '(s, ((c, △), x)) → (s, ((c, out), x)): the carry out of a part, given the carry in',
    part: true
  },
  {
    group: 'adder',
    name: 'carry',
    src: 'rot ; second(swap) ; second(first(unit ; swap)) ; co ; second(first(swap)) ; second(rot) ; xch ; second(second(swap)) ; second(rot†)',
    doc: '(low, c) → (carry into high, (low, c))',
    part: true
  },
  { group: 'adder', name: 'leafd', src: 'first(swap) ; rot ; second(swap) ; rot† ; first(swap)', doc: '((△, ab), c) → ((c, ab), △)', part: true },
  {
    group: 'adder',
    name: 'noded',
    src: 'second(carry) ; xch ; rot† ; second(swap) ; par(self, self) ; second(unit ; swap)',
    doc: 'works out the carry into the high half, then goes down both halves at once',
    part: true
  },
  {
    group: 'adder',
    name: 'down',
    src: 'fix(second(ann†) ; swap ; tri(id, leafd, noded) ; swap ; unit†)',
    doc: '(carry in, (s, x)) → x: each status comes off once it has been used, and the carries go down into the slots'
  },
  { group: 'adder', name: 'carries', src: 'up ; unit ; down', doc: 'fills every carry slot' },
  {
    group: 'adder',
    name: 'addw',
    src: 'prep ; carries ; sumz ; negb ; carries† ; negb ; prep†',
    doc: '<a, b> → <a, a + b>: the carries of a + not(a + b) are those of a + b, so they come back out'
  }
]

export const libTree = new Map<string, V>()
for (const d of LIBRARY) {
  const t = parseProg(d.src)
  define(d.name, t)
  libTree.set(d.name, t)
}

export interface Preset {
  name: string
  prog: string
  input: string
  reads: Reads
  tip: string
}

export const PRESETS: Preset[] = [
  { name: 'rot', prog: 'rot', input: '((T, F), T)', reads: 'bool', tip: 'One move: rot moves a fork from the left to the right, ((u, v), w) becomes (u, (v, w)). Every node is kept; only the links change. Flip it (⇆) and fire again to move it back.' },
  {
    name: 'not',
    prog: 'not',
    input: 'T',
    reads: 'bool',
    tip: 'not is built from moves: an empty slot beside the bit is set where the bit is F, the bit is cleared where the slot stayed F, and the cleared bit is dropped.'
  },
  {
    name: 'cnot',
    prog: 'cnot',
    input: '(T, F)',
    reads: 'bool',
    tip: 'cnot = when(not). tri looks at the left part, takes its stem off while the branch runs, and puts it back: the left is T, so not runs on the right.'
  },
  { name: 'toffoli', prog: 'toffoli', input: '((T, T), F)', reads: 'bool', tip: 'Two controls: tri opens the pair of controls, and cnot runs on the rest when the first is T.' },
  {
    name: 'add',
    prog: 'add',
    input: '(3, 2)',
    reads: 'nat',
    tip: 'Numbers counted in stems. tri takes a stem off a, adds the rest, puts a stem on b, and puts the stem back on a: one stem per step down.'
  },
  { name: 'sub', prog: 'add†', input: '(3, 5)', reads: 'nat', tip: 'add run backward. Each move undoes itself: stem† takes a stem off b, and tri still reads a to know how many.' },
  { name: 'mirror', prog: 'mirror', input: '((△, △△), (△△△, △))', reads: 'tree', tip: 'map runs on a fork’s two halves at once, so every level of the tree mirrors in the same round.' },
  {
    name: 'notw',
    prog: 'notw',
    input: '0b10110010',
    reads: 'bits',
    tip: 'A word is a balanced tree with each bit in a stem. map splits every fork at once, so all 8 bits flip in the same round.'
  },
  { name: 'cnotw', prog: 'cnotw', input: '<0b10110010, 0b01101100>', reads: 'regs', tip: 'Two registers stored bit by bit: cnot at every leaf, all at once.' },
  { name: 'copy', prog: 'copy', input: '(△△, (△, △))', reads: 'tree', tip: 'Copying is a program: dup fills an empty place with x by walking x, stem by stem and fork by fork.' },
  {
    name: 'run a program',
    prog: 'first(unit) ; eval',
    input: '([add], (2, 7))',
    reads: 'nat',
    tip: 'A program is a tree, so it can be data. unit puts (△, ·) on top of add’s tree, which makes add†, and eval runs it: 7 − 2.'
  },
  {
    name: 'addw',
    prog: 'addw',
    input: '<0b1011, 0b0110>',
    reads: 'regs',
    tip: 'Adding in place, (a, b) → (a, a + b), with nothing left over. Statuses go up the tree, carries come down it, then they are all taken back out.'
  }
]
