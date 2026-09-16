# Playground upgrade: one workbench for the landing card and the playground

Delete this file once Phase 3 has landed; the code should then explain itself.

## Goal

The landing card, the playground and the Learn page each run their own copy of
"an editor plus a reduction visualizer". Make them one component set:

1. The visualizer's piece selector reads its pieces from the buffer itself, from
   marked lines of the form `// viz$symbol$: description`, so every example file
   carries its own tour and editing the buffer edits the tour.
2. The landing card can open as a side-by-side workbench (editor left, visualizer
   right, the playground's own layout) instead of flipping between two faces.
3. The playground route hosts the same workbench with its extra chrome (tabs,
   files, share, reset) around it, so nothing is implemented twice.

## What exists today

| Piece | Where | Notes |
|---|---|---|
| Session client | `website/src/lib/disp/client.svelte.ts` | `disp` singleton: `init`, `run`, `evalExpr`, `raw`, `applySpine`, `render`, file ops. Already shared by card and playground. |
| Editor | `website/src/lib/components/DispEditor.svelte` | Props for marks, value blocks, visualize-value, fold sync, jump-to-def, selection action. Already shared. |
| Visualizer | `website/src/lib/components/TreeVis.svelte` | Two postures (`ambient`, `lab`). The cassette tape draws the hardcoded `PIECES` list (line 86) against the toy dictionary `DEFS` in `treecalc.ts:412`; `presets` only feed prev/next and the tip line in lab mode. |
| Landing card | `website/src/lib/components/HeroCard.svelte` | Two faces on a flip: a `DispEditor` with toolbar (example picker, status, Run, Playground link, theatre toggle) and an ambient `TreeVis` with no props. Its `run()` (line 149) re-implements a subset of the playground's outcome-to-marks logic. Theatre is a grid swap owned by `routes/+page.svelte:459-499`. |
| Playground | `website/src/routes/playground/+page.svelte` (1647 lines) | Route-only chrome: toolbar, tabs and files (lines 23-222), share, reset, welcome, memory, toasts. Reusable core: run pipeline (`execute`, `applyOutcome`, `marksFromOutcome`, 476-550), live mode, and the visualizer bridge (`serializeForViz`, `seedViz`, `seedSelection`, `engineStep`, `resolveScopeDef`, `onPodOpen`, `applySeed`, 251-455). Layout: `.stage` flexes `.ed-wrap` beside `.viz-panel` (`clamp(24rem, 42%, 46rem)`), stacking on narrow screens. |
| Examples | `website/src/lib/disp/examples.ts` | Manifest with `kernel` and `landing` flags; sources via `import.meta.glob`; `scripts/validate-examples.mts` runs every file. |
| Learn page | `website/src/lib/learn/chapters/Ch2Trees.svelte:7` | Hardcoded `labPresets`; samples in `code-samples.ts` are validated but not visualized. |
| Math rendering | none | No KaTeX, MathJax or typst in the bundle; `.math` spans are plain styling. |

The seam is already visible: `seedSelection(text)` (playground line 412) is exactly
"take an expression string, resolve its free names against the live scope, seed the
panel". A marked test's left-hand side is such a string.

## The marker format

A marker is a line comment. The symbol between the dollar signs is typst or LaTeX
math for the tape glyph; the description is the tip.

```
// viz$symbol$: description
```

Three placements:

- **On a test line.** The test's left-hand side is the piece's expression.
  ```
  test double 2 = 4 // viz$2 \times$: double, two applications of add
  ```
- **On its own line, with a code span.** For pieces that cannot be tests, such as
  symbolic reductions over free names. The first backtick span is the expression;
  names that do not resolve in scope stay free variables, which the visualizer
  already draws as fruit.
  ```
  // viz$K$: `K x y` keeps the first argument and discards the second
  ```
- **Directly above a test**, on its own line and without a code span: attaches to
  the next test. Reads better for long tests.

Optional flags in square brackets after the symbol, none required:
`// viz$+$[fast]: add, the recursion storm` (fast = the `stepMs: 90` the `add`
piece uses today). Add flags only when a real piece needs one.

Glyph rendering: a small in-house subset renderer, `Glyph.svelte`, not KaTeX.
The subset is identifiers and digits, `_{...}` and `^{...}` (single characters may
omit the braces), and a symbol table (`\triangle` △, `\neg` ¬, `\wedge` ∧, `\vee`
∨, `\lambda` λ, `\to` →, `\times` ×, `\circ` ∘). Unknown commands render as their
name so nothing ever breaks the tape. The cassette already renders subscripts with
`<sub>`; `Glyph` replaces the `sym`/`sub` pair. KaTeX (about 270 KB with fonts) can
be swapped in behind the same component if the subset ever stops sufficing.

The scanner lives in `website/src/lib/disp/viz-marks.ts`:

```ts
export interface VizPiece { glyph: string; expr: string; tip: string; line: number; flags: string[] }
export function vizPieces(source: string): VizPiece[]
```

It is a line scan, not a parse: `test <lhs> = <rhs> // viz$…$: …` and
`// viz$…$: …` with an optional first code span. `validate-examples.mts` gains a
check that every marked expression parses with `parseTree` and that a marker without
a code span sits on or above a test.

## The landing tour becomes an example file

Today's ambient pieces (`PIECES`, TreeVis line 86) move into a new raw-prelude
example, `examples/tree-calculus.disp`, listed first among the landing examples so
the first paint stays instant (no kernel). It defines its pieces as explicit trees,
the way the toy dictionary does, so the tour stays as small as it is now:

```
open use raw "../prelude.disp" {}

K := t t

// viz$\triangle$: `t x` a leaf applied builds a stem
// viz$s$: `t x y` a stem applied builds a fork
// viz$K$: `K x y` keep the first argument, discard the second
// viz$F_t$: `t (t a b) c t` the argument is a leaf → the first branch (a)
// viz$F_s$: `t (t a b) c (t u)` the argument is a stem → second branch on its child (b u)
// viz$F_f$: `t (t a b) c (t u v)` the argument is a fork → third branch on its children (c u v)
test not true = false // viz$\neg$: negation, one F rule on the argument's shape
test and false true = false // viz$\wedge$: conjunction
```

`not`, `and`, `true` and `false` are the raw prelude's (`lib/prelude.disp:57-109`);
`S` and `add` come along the same way once their explicit trees are copied over
from `treecalc.ts`. The visualizer runs in the playground posture everywhere
(`lazyParse`), which holds every application as a step, so the `lazyTop` flag on
the stem and fork pieces is no longer needed.

The kernel examples (`hello-card.disp`, `records.disp`, `proofs.disp`,
`universe.disp`) get markers on the tests worth watching, for example
`test quadruple 3 = 12 // viz$4 \times$: two doubles`. Their names resolve only after
the buffer has run, so the card seeds the tape after the first successful run and
shows free-name fruit before that.

## Components after the migration

```
routes/+page.svelte           routes/playground/+page.svelte
   HeroCard                       tabs · files · share · reset · welcome · toasts
      Workbench (card|split)         Workbench (full)
         DispEditor                     DispEditor
         VizPanel                       VizPanel
            TreeVis                        TreeVis
```

- **`lib/disp/marks.ts`**: `marksFromOutcome(out)` extracted from the playground.
  `HeroCard.run` uses it too, which also gives the card the inline value blocks it
  lacks today.
- **`lib/disp/viz-bridge.svelte.ts`**: the value-to-visualizer plumbing extracted
  from the playground (`rawToT`, `tToRaw`, `serializeForViz`, `seedViz`,
  `seedSelection`, `engineStep`, `resolveScopeDef`, `onPodOpen`, `applySeed`,
  `POD_MAX_NODES`). One instance per workbench, holding `viz`, `vizApi`, `vizCtl`.
  It also owns `seedPiece(piece)`, which is `seedSelection(piece.expr)`.
- **`components/VizPanel.svelte`**: the playground's `.viz-panel` aside (close
  button, height measurement, the `TreeVis` with the bridge's props) plus the tape
  fed by `pieces`. Used by both hosts.
- **`components/Workbench.svelte`**: `DispEditor` + `VizPanel` + the run pipeline
  (`execute`, `applyOutcome`, live re-check, `canAutoRun`) + the bridge. Props:
  `doc`, `layout: 'card' | 'split' | 'full'`, `pieces` (rescanned from the doc on a
  debounce), `path` (the run path, the playground's per-tab path), `toolbar`
  snippet (the host supplies its own toolbar contents), and an `api` (`setDoc`,
  `run`, `runToCursor`, `openViz`, `seedPiece`). Layout `card` shows the editor
  only; `split` shows editor and panel side by side (stacked on narrow screens,
  the playground's media query); `full` is `split` sized by the app-mode shell.
- **`TreeVis.svelte`**: gains `pieces: VizPiece[]`; the cassette, prev/next, the
  auto-cycle and the tip line all read it. `PIECES` and `DEFS` are deleted once
  `tree-calculus.disp` carries the tour (`DEFS` stays only if a test still needs it).
  Glyphs render through `Glyph`.
- **`HeroCard.svelte`**: keeps the flip and the theatre toggle, adds the split
  toggle. Compact and theatre keep the two faces (code face = `Workbench` in
  `card` layout; visualizer face = the same `TreeVis` with the buffer's pieces).
  Split hides the flipper and the face picker and shows one `Workbench` in `split`
  layout at theatre size; the page's grid already knows the theatre sizing
  (`min(72vh, 46rem)`), so split rides the existing `theatre` grid mode with a
  `split` class on the card.
- **Playground route**: becomes chrome around `Workbench full`. Tabs and files stay
  where they are; `openViz` from a value block goes through the bridge as now.

## Phases

**Phase 0, no behaviour change: extract.**
- [ ] `marks.ts`; the playground and `HeroCard.run` both use it.
- [ ] `viz-bridge.svelte.ts`; the playground uses it. `svelte-check`, `npm run
      validate:examples`, and a manual pass of the playground (value pop-out,
      selection action, pod open, engine step, fold sync) stay green.

**Phase 1: pieces from the buffer.**
- [ ] `viz-marks.ts` scanner and `Glyph.svelte`.
- [ ] `TreeVis` `pieces` prop; the cassette, prev/next and auto-cycle read it.
- [ ] `examples/tree-calculus.disp` with the marked tour, first in the landing
      list; markers on the kernel examples' tests.
- [ ] `HeroCard` rescans the buffer on edit and hands `pieces` to its visualizer
      face; seeds through the bridge after a run.
- [ ] `validate-examples.mts` checks the markers.
- [ ] Delete `PIECES`; delete `DEFS` if nothing else reads it.

**Phase 2: the workbench.**
- [ ] `VizPanel.svelte` and `Workbench.svelte`; the playground route hosts
      `Workbench full`.
- [ ] `HeroCard`'s code face hosts `Workbench card`; the split toggle enters
      `theatre` and shows `Workbench split`.
- [ ] The playground panel gets the tape too (pieces from the active buffer).

**Phase 3: the Learn page.**
- [ ] `Ch2Trees` presets become markers in its samples (`code-samples.ts`), read
      by the same scanner. Delete this file.

## Validation

`npm run check`, `npm run validate:examples`, `npm run build` (its snapshot
self-check runs the landing card example), and a manual pass on the landing card
in compact, flipped, theatre and split, plus the playground's tabs, files, share
and reset. The kernel snapshot only covers `lib/` and the archive, so example
changes do not rebuild it.

## Open questions

- Should split mode be a third state next to theatre, or should theatre simply
  become split? Keeping both is cheap; theatre-as-two-faces still suits the tour.
- Does the tape want the glyph or a short word when an example has no natural
  symbol? The scanner could accept `viz$$` and fall back to the piece's index.
- The symbolic pieces need `lazyParse`; the kernel examples want it too. Confirm the
  Learn page's lab instrument reads well with every application held as a step.
