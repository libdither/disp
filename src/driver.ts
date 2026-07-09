// The impure driver (EFFECT_BOOTSTRAP_PLAN § "The impure driver, mapped").
//
// The ONE impure component of the effects story: a host loop that walks a
// fully-handled program value and performs its residual IO ops against the
// world, exactly once each. Everything else stays pure: handlers fold, the
// driver performs; a test run replaces the driver with mock handlers
// in-language and never constructs this file's loop at all.
//
// Contract: the loaded file exports `main` with a VERIFIED Eff annotation
// (`main : Eff io_row X := ...`) — the kernel already checked the program
// against its row at load (the elaborator's deferred auto-verification), so
// the driver's own checks are the operational half only:
//   - the annotation must BE an Eff type (former-signature compare);
//   - each performed op's label must be in the declared row (label_in_row,
//     run in-language when the kernel is in the session's module cache);
//   - each op's argument must be CLOSED (is_closed — a neutral reaching the
//     boundary is a checking artifact leaking into the world; abort).
//
// The loop: classify one node. Pure x => exit with x. Op op arg k => decode
// the label (host-side string decode of the pair(effect, op) tags), perform
// via the host op map, encode the answer r, continue as apply(k, r). Async
// answers resume the pure world exactly once per answer (read_line awaits).

import { createInterface, type Interface } from "node:readline"
import type { Tree } from "./eval/eager.js"
import type { Session } from "./eval/types.js"
import type { Decl } from "./compile.js"
import { elab, B, moduleCacheBySession } from "./elab/state.js"
import { stringToTree, treeToString, treePairFst, accTree } from "./elab/literals.js"

export interface DriveResult { performed: number; exit: Tree }

// Best-effort lookup of a kernel/std export by name in the session's module
// cache (the trees `use`d modules exported). Absent when the loaded file
// never opened a module exporting the name.
function findExport(session: Session<Tree>, name: string): Tree | undefined {
  const cache = moduleCacheBySession.get(session)
  if (!cache) return undefined
  for (const e of cache.values()) {
    const i = e.fields?.indexOf(name) ?? -1
    if (i >= 0 && e.fieldTrees?.[i] != null) return e.fieldTrees[i]
  }
  return undefined
}

export function findMain(decls: Decl[]): Extract<Decl, { kind: "Def" }> | undefined {
  const d = decls.find(d => d.kind === "Def" && d.name === "main")
  return d && d.kind === "Def" ? d : undefined
}

export async function driveMain(session: Session<Tree>, decls: Decl[]): Promise<DriveResult> {
  const main = findMain(decls)
  if (!main) throw new Error("driver: no `main` export in this file")
  if (main.type == null) throw new Error("driver: `main` must carry an Eff annotation (main : Eff io_row X := ...)")
  const prevCs = elab.cs
  elab.cs = session
  let rl: Interface | null = null
  try {
    const leaf = session.leaf()
    const isTrue = (h: Tree) => session.classify!(h).tag === "leaf" // true = the leaf (§2.7)
    const fork = (h: Tree) => {
      const c = session.classify!(h)
      return c.tag === "fork" ? c : null
    }

    // The annotation must be an Eff type: compare the wait-form's constant
    // former signature against a reference application. (Eff is now the single
    // eliminable recognizer — the former inert Eff / gated EffE split collapsed.)
    const sigOf = (t: Tree | undefined) => (t == null ? null : treePairFst(session.apply(session.apply(t, leaf, B()), leaf, B())))
    const mainSig = treePairFst(main.type)
    const effSig = sigOf(findExport(session, "Eff"))
    const isEff = mainSig != null && effSig != null && session.equal!(mainSig, effSig)
    if (!isEff) throw new Error("driver: `main`'s annotation is not an Eff type (open std/effect.disp and annotate main : Eff io_row X)")

    // The declared row, read through the record cuts: the type's meta record
    // holds recognizer_params = { row; result } (make_meta's shape).
    const w1 = fork(main.type)
    const w2 = w1 && fork(w1.right)
    if (!w2) throw new Error("driver: cannot read main's type metadata")
    const rp = session.apply(w2.right, accTree("recognizer_params"), B())
    const row = session.apply(rp, accTree("row"), B())

    // The static half, run here because a ROOT file's own exports are not
    // self-verified at load (module exports verify when the file is used):
    // check main against its declared Eff type through the kernel — the deep
    // row certificate — before performing anything.
    const paramApply = findExport(session, "param_apply")
    const okTree = findExport(session, "Ok")
    const trueTree = findExport(session, "true")
    if (paramApply && okTree && trueTree) {
      const verdict = session.apply(session.apply(paramApply, main.type, B()), main.tree, B())
      if (!session.equal!(verdict, session.apply(okTree, trueTree, B())))
        throw new Error("driver: main does not inhabit its declared Eff type (the kernel's deep row certificate failed)")
    }

    // In-language guards, when the kernel is in the cache.
    const labelInRow = findExport(session, "label_in_row")
    const isClosed = findExport(session, "is_closed")

    // The host op map: label tree -> perform. This map IS the driver's
    // capability set; an op outside it refuses at run time.
    const pairT = (a: string, b: string) => session.fork(stringToTree(a), stringToTree(b))
    const unitVal = session.fork(leaf, leaf) // unit_val = t t t = fork(t, t)
    const readLine = async (): Promise<string> => {
      if (!rl) rl = createInterface({ input: process.stdin, crlfDelay: Infinity })
      return await new Promise<string>(resolve => {
        const iface = rl!
        const onLine = (line: string) => { cleanup(); resolve(line) }
        const onClose = () => { cleanup(); resolve("") }
        const cleanup = () => { iface.off("line", onLine); iface.off("close", onClose) }
        iface.once("line", onLine)
        iface.once("close", onClose)
      })
    }
    const opMap: { label: Tree; perform: (arg: Tree) => Promise<Tree> }[] = [
      { label: pairT("IO", "print"), perform: async arg => { process.stdout.write(treeToString(arg) + "\n"); return unitVal } },
      { label: pairT("IO", "read_line"), perform: async () => stringToTree(await readLine()) },
    ]

    const strPure = stringToTree("Pure")
    const strOp = stringToTree("Op")
    const labelStr = (label: Tree) => {
      const c = fork(label)
      return c ? `${treeToString(c.left)}/${treeToString(c.right)}` : "<unreadable label>"
    }

    let cur = main.tree
    let performed = 0
    for (;;) {
      const node = fork(cur)
      if (!node) throw new Error("driver: program node is not Pure/Op (main did not evaluate to a computation)")
      if (session.equal!(node.left, strPure)) return { performed, exit: node.right }
      if (!session.equal!(node.left, strOp)) throw new Error("driver: unknown program node tag")
      const p1 = fork(node.right)
      const p2 = p1 && fork(p1.right)
      if (!p1 || !p2) throw new Error("driver: malformed Op node")
      const op = p1.left, arg = p2.left, k = p2.right
      const label = session.apply(op, accTree("label"), B())
      // Rows contain EFFECT names (eff_check keys on effect_of op = the
      // label's first component), not full labels.
      const effName = fork(label)?.left
      if (labelInRow && effName != null && !isTrue(session.apply(session.apply(labelInRow, effName, B()), row, B())))
        throw new Error(`driver: op ${labelStr(label)} is outside main's declared row`)
      if (isClosed && !isTrue(session.apply(isClosed, arg, B())))
        throw new Error(`driver: op ${labelStr(label)} carries an open value (a checking artifact leaked to the boundary)`)
      const entry = opMap.find(e => session.equal!(e.label, label))
      if (!entry) throw new Error(`driver: no host handler for op ${labelStr(label)}`)
      const r = await entry.perform(arg)
      performed++
      cur = session.apply(k, r, B())
    }
  } finally {
    if (rl) (rl as Interface).close()
    elab.cs = prevCs
  }
}
