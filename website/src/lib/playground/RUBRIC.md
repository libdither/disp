# Playground review rubric

What a reviewer scores when judging the playground page (`/playground/`) as a whole. It is
written for council rounds (three reviewers on different models score the same build, the
fixes land, and a fresh round re-scores), so every round runs the same scenarios and the
scores can be compared.

## Setup

- The page: http://functor.ts.zyancraft.net:5174/playground/ (dev server, serves the main checkout; `scripts/dev-server.sh` starts it).
- Filmstrips of the tree viewer: `npm run film -- <name> <line-regex> [steps] [times ms] [program file] [parallel|dark|play]`
  from `website/`. Every step gets one row of shots taken during its animation (default 0, 120, 240, 380, 620 ms),
  written to `$FILM_OUT` (default `/tmp/playground-film`). It also prints each step's caption and any console errors.
  With no program file it loads `swap := {f, x, y} => f(y, x)`. The line can be anywhere in the program.
  `play` presses Play instead and tiles `<steps>` shots taken every `<first time>` ms.
- The film script's `slow` option makes every animation 10 times slower so its shots land where the times say, and `try` films
  Try it. `npm run motion-check` steps real reels through `motion.ts` without a browser and checks that no node turns back, the
  trunk and zoom hold still, nothing leaves the view and a knocked-off variable falls under gravity, never below the
  ground, and comes to rest on it near the trunk; `-- --svg <reel> <step>`
  draws that step's paths.
- For anything the film script doesn't cover (hover, play, scrubbing, phone, first visit), drive the page with
  `playwright-core` the way the film script does; it is a dev dependency of `website/`.
- The code: `website/src/routes/playground/+page.svelte` and `website/src/lib/playground/`
  (`Inspector.svelte` runs the step reel, `TreeCanvas.svelte` draws it, `motion.ts` works out where everything is at each moment of a step
  and where the camera is, `guides.ts` says where things go during a step,
  `trace.ts` and `store.ts` choose and perform the steps, `roles.ts` works out what each part of a finished function does to each input).
- Reviews are read-only: put captures in `/tmp/council-<you>/`, never edit the repo.

## Scenarios

Run all of them every round. Light theme at 1500×1000 unless stated otherwise.

| | Scenario | How |
|---|---|---|
| A | First visit | Fresh storage, so the welcome card shows. Read it and the default program as a newcomer would. |
| B | Binders, one at a time | `swap` in the film script, 6 steps: S split, eta, K. |
| C | Binders in parallel | The same with `parallel`. |
| D | S and K from the default program | Cursor on `S := {f, g, x} => f(x, g(x))`, then `K2 := {x, y} => x`. |
| E | A long run | The test `add(2, 3) = 5`. Use Play, change the speed, drag the timeline, jump between the binder and run phases. |
| F | Held applications | The test `wait(and, true, false) = false`. |
| G | Interruptions | Press → five times fast, press ← halfway through an animation, click a ringed node, and move the cursor to another definition while playing. |
| H | Styles and themes | Pause in the middle of E and switch through every tree style (tree, moss, garden, quiet, ink, bold) in light and dark. |
| I | Phone | 420×860: the Outline / Code / Tree tabs, and stepping a tree. |
| J | Expressions | Type `add(1, 2)` into "Try an expression", then pin it. |
| K | Editing | Break a definition, fix it, use autocomplete, hover, the inlay parameter hints, F12 and the outline. |

## Categories

Score each category from 0 to 10. The overall score is the weighted mean, rounded to one decimal.

What the numbers mean everywhere: **10** nothing a picky first-time user would notice; **8** small polish only; **6**
works, but has one real problem a newcomer would hit; **4** several real problems, or one that blocks understanding;
**2** broken.

### 1. Animation (weight 40)

The tree viewer exists to show *where things go* in a rewrite. Score the motion on these points:

- **The rule is visible in the motion.** S split: the binder's copies slide down both sides. K: the dropped
  argument falls away. `{x} => x`: the binder and its variable merge into I. Eta: the function rises into place. A tree
  S step: the shared argument is visibly copied. The triage rules: you can tell which of leaf / stem / fork was matched.
  Could someone who doesn't know the rule guess it from three watches?
- **Identity is kept.** After a step, can you point at each node and say which node it was before? Parts that don't
  take part in the step stay still, unless the layout has to move them, and then they glide rather than jump. The view
  doesn't jump around or rescale for no reason.
- **One focal point.** The eye goes to the site before or as the motion starts. Nothing else competes (other rings
  or fades).
- **Timing.** A single step reads at the default speed without feeling slow (roughly 300–600 ms). Easing feels
  physical. Nothing pops in or out: new parts fade or grow in, and removed parts fade out toward where they went. Play
  mode keeps a rhythm you can follow, and parallel mode is still readable with several sites firing at once.
- **Interruption.** Rapid stepping, stepping back, scrubbing and switching definitions mid-animation all end on the
  correct frame, with no leftover ghosts, no doubled nodes and no animations piling up.
- **Restraint.** No effect is there for its own sake. Motion is the guide, not the show.
- **Smoothness.** No dropped frames on a tree of about 200 nodes. `prefers-reduced-motion` is respected.

### 2. Reading the tree (weight 15)

Captions, hover text, the legend, rule names, the rings marking the next step, the phase buttons and the step count.
Can a newcomer predict the next step? Does the text agree with what the animation shows, and with how the reel
actually works now (binders and the run share one reel)?

### 3. Editor (weight 12)

Re-checking speed, values and verdicts on their lines, diagnostics, autocomplete, hover, inlay hints, go-to-definition
and the outline. Does what the editor says agree with the tree panel?

### 4. Visual design and layout (weight 12)

Hierarchy, density, alignment, typography and colour across the five tree styles and both themes. Does the page match
the rest of the site? Are the empty states ("Put your cursor on a definition") useful? Is the toolbar's icon row
understandable without hovering over each icon?

### 5. The first five minutes (weight 8)

The welcome card, the default program, Examples, and how long until a newcomer sees a tree animate and understands why.

### 6. Phone (weight 5)

Tabs, touch targets, whether the tree fits, whether stepping is possible without a keyboard.

### 7. Accessibility (weight 4)

Keyboard-only use, focus rings, contrast in every style, labels on icon buttons, and reduced motion.

### 8. Robustness and speed (weight 4)

Console errors, load time, large or long reels, the fallback to the two-phase reel, and memory use over a long Play.

## Hard fails

Any of these caps the overall score at 5 until it is fixed: a console error in a scenario, a wrong tree or
result, an animation that leaves stale nodes behind, page text that contradicts what the page does, or a scenario that
can't be completed on the phone.

## What to hand back

1. The overall score out of 10 and each category's score, each with a one-line reason.
2. At most 12 findings, most severe first. Each one has:
   - its severity (blocker, major, minor or polish), category and scenario
   - evidence: a screenshot or filmstrip path and how to reproduce it
   - one suggested fix in a sentence, and its size (S, M or L)
3. Separately, at most 3 ideas that go beyond this page's scope, as ideas rather than findings.

## Scope

Findings must be fixable inside the playground's own files. No changes to the compiler, the language, the evaluator or
other pages of the site, and no new large features. A finding only counts if a user would notice it. Keep going round
by round until every reviewer scores 9 or above, or only polish is left.
