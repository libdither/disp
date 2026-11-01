// The player's compiler (bundled into ../player/lin-compiler.js by bundle.mts): `PROGRAM @ VALUE` in the
// website's two-way language, the library's names included, to the text the engine loads after `lin:`
// (crate/src/linear.rs): the net, the interpreter's answer and the input.
import { run, show } from '../../../website/src/lib/evaluators/twoway/core.ts'
import { parseProg, parseVal } from '../../../website/src/lib/evaluators/twoway/text.ts'
import '../../../website/src/lib/evaluators/twoway/library.ts'
import { load, serialize } from './compile.ts'

function linCompile(src: string): { text: string; agents: number; calls: number } {
  const at = src.lastIndexOf('@')
  if (at < 0) throw new Error('lin: PROGRAM @ VALUE, e.g. lin: notw @ 0b1011')
  const p = parseProg(src.slice(0, at))
  const x = parseVal(src.slice(at + 1))
  const want = run(p, x)
  const { net, calls } = load(p, x)
  return { text: `${serialize(net)}#${want ? show(want) : 'stuck'}#${show(x)}`, agents: net.ag.size, calls }
}

;(globalThis as unknown as { LinCompile: typeof linCompile }).LinCompile = linCompile
