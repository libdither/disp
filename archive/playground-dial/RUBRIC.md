# Review rubric: the detail dial design

For a reviewer scoring `DESIGN.md` and the mock in this folder. Read the design, then use the mock
(`archive/playground-dial/index.html` on the dev server that `scripts/dev-server.sh` starts, `?ex=fan|check|twice`). Drive it by hand
with playwright-core (`node archive/playground-dial/shoot.mjs <example> <out-prefix> '<actions json>'`, actions listed
at its top; `window.__dial` exposes forward/back/setLevel). Put captures in `/tmp/council-dial-<round>/`. Don't edit
the repo.

The question the design answers: *a reader wants more or less information about part of a running program; how do
they say so, and what do they get?* Judge it as a design for disp's real playground (`website/src/lib/playground/`),
not as a polished mock. A rough edge in the mock counts only if it shows a flaw in the design.

## Criteria (score each 0–10; overall = weighted mean)

1. **Matches intent (25).** Does turning the dial do what someone means by "more/less here"? Are the defaults right for
   a newcomer, for someone learning the rules, and for someone debugging their own function? Does the suggestion
   logic read intent well, without nagging or guessing wrong?
2. **Understandable (20).** Can a first-time user predict what a turn will do before doing it? Is "what the dial
   covers" visible? Are the four settings' names and the captions clear? One concept, or several in disguise?
3. **Animation (20).** Does a hand-off beat make the S/K/I route readable as "where the argument goes"? Does a Result
   beat read as "the call became its result"? Is identity kept, is it free of clutter, and does it keep one focus?
4. **Coherent model (15).** Are the composition rules small and principled? Do the edge cases follow from them
   (calls inside calls, laziness, tail calls, half-applied code, constants, Skip riding along)?
5. **Control (10).** Nothing changes behind your back; every change can be undone; there is an overview (side list,
   timeline); keyboard, mouse and touch are all covered.
6. **Feasible in the playground (10).** Does it map onto the trace, guides and layout code with bounded cost? Does it
   handle tree calculus specifics: triage, hash-consing, binder steps, parallel mode, big reels like `add(2, 3)`?

Anchors: **10** nothing a picky expert designer would change; **8** small polish only; **6** a real problem a user would
hit; **4** several, or one that sinks the idea; **2** doesn't work.

## Hand back

1. Overall score and per-criterion scores, each with one line of reasoning.
2. At most 10 findings, most important first: severity (major/minor/polish), criterion, evidence (screenshot path or
   quote), and one suggested fix with a size (S/M/L).
3. At most 3 ideas outside the design's scope.

Stay inside the design's own question. "Add a different feature" is not a finding unless the design fails without it.
