// Writes cases.bend: small trees, the small programs of swapnet's kind, and core.ts's own answers for
// library programs run both ways on them, as Bend values, so LAWS.bend can hold the Bend port to core.ts.
// Rerun after core.ts or library.ts change:
// `npx --prefix website tsx research/signal-fabric/frame/cases.mts`
import { writeFileSync } from 'node:fs'
import { F, L, S, nat, prog, run, size, type V } from '../../../website/src/lib/evaluators/twoway/core.ts'
import { define, parseProg } from '../../../website/src/lib/evaluators/twoway/text.ts'
import '../../../website/src/lib/evaluators/twoway/library.ts'
import { ADDB, ADDT, register, words } from './binary.ts'

define('addb', parseProg(ADDB))
define('addt', parseProg(ADDT))

const lit = (v: V): string => (v.tag === 'leaf' ? 'TW.L{}' : v.tag === 'stem' ? `TW.S{${lit(v.c)}}` : `TW.F{${lit(v.l)}, ${lit(v.r)}}`)
const list = (xs: string[]) => `[\n    ${xs.join(',\n    ')}]`
// the laws only hold the design to the reference, so first make sure addb adds
for (const n of [1, 2, 3]) for (let a = 0; a < 1 << n; a++) for (let b = 0; b < 1 << n; b++) {
  const y = run(parseProg('addb'), register(a, b, n), true, 100_000), want = register(a, (a + b) % (1 << n), n)
  if (!y || lit(y) !== lit(want)) throw new Error(`addb gives ${a} + ${b} wrong in ${n} bits`)
}
// addt, on trees, is not in the design yet; it only has to add
for (const n of [1, 2, 4]) for (let a = 0; a < 1 << n; a++) for (let b = 0; b < 1 << n; b++) {
  const y = run(parseProg('addt'), words(a, b, n), true, 1_000_000)
  if (!y || lit(y) !== lit(words(a, (a + b) % (1 << n), n))) throw new Error(`addt gives ${a} + ${b} wrong in ${n} bits`)
}

/** Every tree of at most n nodes. */
function trees(n: number): V[] {
  if (n < 1) return []
  const out: V[] = [L()]
  for (const t of trees(n - 1)) out.push(S(t))
  for (let k = 1; k < n - 1; k++) for (const a of trees(k)) for (const b of trees(n - 1 - k)) if (size(a) + size(b) === n - 1) out.push(F(a, b))
  return n === 1 ? out : [...new Map([...trees(n - 1), ...out].map((t) => [lit(t), t])).values()]
}
const small = trees(7)

// swapnet's programs: id, swap, and one level of †, ; and map over them, a few deeper ones, and recursions
// that loopnet copies at forks
const base = [prog.id(), prog.atom('swap')]
const fragment = [...base, ...base.map(prog.dag), ...base.flatMap((a) => base.map((b) => prog.seq(a, b))), ...base.flatMap((a) => base.flatMap((b) => base.map((c) => prog.map(a, b, c))))]
for (const src of ['first(first(swap))', 'second(first(swap)) ; swap', '(first(swap) ; swap)†', 'map(map(swap, id, id), swap, second(swap))', 'par(swap, first(swap)) ; swap',
  'mirror', 'mirror†', 'fix(map(self, id, self))', 'fix(map(id, self, self) ; swap)', 'first(mirror) ; swap', 'mirror ; first(swap)'])
  fragment.push(parseProg(src))

// loopnet's programs past swapnet's: stems made and taken away, and loops (add, add† and friends, and two
// followed by a step into their part), and
// the trees to run them on: pairs of numbers (counted in stems), and a few that are not numbers
const loops = ['add', 'add†', 'stem', 'stem†', 'second(stem)', 'first(stem†) ; swap', 'swap ; stem', 'second(stem) ; add',
  'tri(id, second(stem), id)', 'tri(second(stem), id, id)', 'fix(tri(id, second(stem) ; self, id))',
  'fix(tri(second(stem), self ; second(stem), id))', 'map(id, add, add)', 'fix(tri(id, self ; first(swap), id))',
  'add ; swap', 'add ; second(stem)'].map(parseProg)
const numbers: V[] = []
for (let a = 0; a <= 3; a++) for (let b = 0; b <= 3; b++) numbers.push(F(nat(a), nat(b)))
numbers.push(F(F(L(), L()), nat(2)), F(nat(2), F(L(), L())), S(F(nat(1), L())), F(F(nat(1), nat(2)), F(nat(2), nat(1))), F(S(S(F(L(), S(L())))), nat(1)))

// loopnet's binary programs: not, the tri that flips on a 1, steps after loops, a fix a fork runs, and
// binary add (binary.ts) both ways; on bits, register pairs of 1 and 2 bits, and trees that hold no bit
// where one should be (not is stuck there)
const binary = ['not', 'second(not)', 'first(not) ; second(not)', 'tri(id, second(not), id)', 'tri(id, second(not), id) ; swap',
  'tri(id, second(tri(id, second(not), id)), id)', 'fix(first(not) ; second(self))', 'fix(second(self) ; first(not))', 'addb', 'addb†'].map(parseProg)
const registers: V[] = [L(), S(L()), S(S(L())), F(L(), S(L())), F(S(L()), S(L())), F(S(L()), F(L(), S(L()))), F(S(L()), F(S(S(L())), L())), F(F(L(), L()), S(L())), F(S(L()), F(L(), L()))]
for (const n of [1, 2]) for (let a = 0; a < 1 << n; a++) for (let b = 0; b < 1 << n; b++) registers.push(register(a, b, n))

// core.ts's answers: library programs and the fragment, forward and backward, on the small trees. Each
// program is a def of its own, and the cases come in chunks: Bend expands a list literal on its stack.
const LIB = ['swap', 'cross', 'sink', 'stem', 'unit', 'rot', 'xch', 'transpose', 'mirror', 'not', 'cnot', 'add', 'copy']
const progs = [...LIB.map((n) => [n, parseProg(n)] as const), ...fragment.map((p, i) => [`fragment_${i}`, p] as const)]
const cases: string[] = []
for (const [name, p] of progs)
  for (const x of small)
    for (const fwd of [true, false]) {
      let y: V | null
      try { y = run(p, x, fwd, 10_000) } catch { continue } // too long: no case
      cases.push(`TW.Case{p_${name}(), ${lit(x)}, ${fwd ? 'True{}' : 'False{}'}, ${y ? `Some{${lit(y)}}` : 'None{}'}}`)
    }
const CHUNK = 250
const chunks = Array.from({ length: Math.ceil(cases.length / CHUNK) }, (_, i) => cases.slice(i * CHUNK, (i + 1) * CHUNK))

writeFileSync(new URL('./cases.bend', import.meta.url), `# Generated by cases.mts from core.ts: do not edit.

import Base
import ./twoway.bend as TW

# every tree of at most 7 nodes
def trees() -> +List<TW.Tree>:
  ${list(small.map(lit))}

# loopnet's programs past swapnet's
def loops() -> +List<TW.Tree>:
  ${list(loops.map(lit))}

# pairs of numbers up to 3, and four trees that are not
def numbers() -> +List<TW.Tree>:
  ${list(numbers.map(lit))}

# loopnet's binary programs
def binary() -> +List<TW.Tree>:
  ${list(binary.map(lit))}

# bits, register pairs of 1 and 2 bits, and trees with no bit where one should be
def registers() -> +List<TW.Tree>:
  ${list(registers.map(lit))}

${progs.map(([name, p]) => `def p_${name}() -> TW.Tree:\n  ${lit(p)}\n`).join('\n')}
# programs of swapnet's kind: id, swap, †, ; and map
def programs() -> +List<TW.Tree>:
  ${list(fragment.map((_, i) => `p_fragment_${i}()`))}

${chunks.map((c, i) => `def core_${i}() -> +List<TW.Case>:\n  ${list(c)}\n`).join('\n')}
# core.ts's answers (None: stuck) for ${LIB.join(', ')} and the programs above, both ways, on the trees above
def core() -> +List<TW.Case>:
  List.concat(&2, TW.Case, ${list(chunks.map((_, i) => `core_${i}()`))})
`)
console.log(`${small.length} trees, ${fragment.length} programs, ${cases.length} cases in ${chunks.length} chunks`)

// the adder alone, for score.bend, which cases.bend is too big to build into
writeFileSync(new URL('./binary.bend', import.meta.url), `# Generated by cases.mts from binary.ts: do not edit.

import Base
import ./twoway.bend as TW

# addb, binary add (binary.ts)
def p_addb() -> TW.Tree:
  ${lit(parseProg('addb'))}
`)
