# Block design search: shared brief

Goal: find the most elegant reversible 2×2 block automaton whose grid coarse-grains to trees and runs the
two-way calculus (`calculus.py`, mirror of `../calculus.bend`). One agent per design family; each climbs as high as it
can under the same laws, and reports honestly. A negative result with the reason is a good result.

## Tools
- `laws.py` holds the laws (its docstring says what each restricts and leaves free) and the harness. Run a design:
  `systemd-run --user --scope -q -p MemoryMax=1500M -p MemorySwapMax=200M timeout 600 python laws.py <family>/design.py [--upto RUNG] [--quick]`
  from this directory. Every run needs that memory cap and a timeout. Kill processes by PID only.
- `calculus.py`: the calculus, the programs, and the ladder of rungs: swaps, maps, grow, moves, tri, fix, add, general (moves must compose).
- `physics.py`: sparse simulator on an unbounded plane (grows right and down; anything at x<0 or y<0 has left).
- `smoke/design.py`: the smallest example of the design API; it only runs id.
- Do not change a law to suit your design. If a law blocks something good, keep to it and say so in your report, with
  what you would change and why. A plain bug in the harness may be fixed in place with a minimal edit (others share the
  file), and must be listed in your report.
- Work only in `<family>/`. No commits. No files elsewhere. Python is `python`.
- The rule must be a plain reaction list (a generator is fine, and encouraged: write few shapes, expand by symmetry
  and by token names). `lay`, `link`, `node` stay simple functions: the winner gets ported to Bend.

## What wins
In order: highest rung with all hard laws; then few reaction shapes (and few reactions); few cell states; how ticks
scale (`speed` in laws.py); graded laws held. The previous design for scale: 89 reactions, 8,064 cell states, a compiled
tape, passes swaps and maps, fails grow (`stem;stem;stem` never ends). Beating it on rule count while reaching further
is the point. Prefer one idea that does several jobs to one mechanism per job. If the count climbs, suspect the
representation. Stop when you can't climb further after real attempts, or after about three redesigns of your family.

## Facts worked out on paper (test them, don't trust them; each may be wrong or may not suit your family)
Corrections from the search: in fact 1 a bounce is free only for a token with nothing to undo (a token that turns back un-runs its work, and can't park). Fact 3's slide doesn't repeat past one row: parent and child right arms collide in the shared column, so whole parts move instead. Fact 6's one-block rotation doesn't fit when sites are 2 apart.
1. Heading by phase. A reaction that swaps two linked wire cells' tokens moves a token one cell a tick. A token at
   the even-column cell of a horizontal pair on an even tick moves right; at the odd-column cell it moves left. So no
   direction field is needed: heading is (position + tick) parity. A token that fails to move for one tick reverses, so a dead
   end reflects it for free. Two tokens head-on just swap, which means they pass through each other. Running time
   backward is the same rule from the other phase. A diagonal swap in a block gives a straight diagonal flight (billiard
   balls: no wire needed).
2. Wire geometry. Each orthogonal edge lies in exactly one active block, so a wire must alternate even-block and
   odd-block edges: at each cell it can go straight or turn to one fixed side. Treat (even, even) cells as sites: a
   site's even block holds it, its E cell and its S cell (and the (odd, odd) cell); its odd block holds it, its N cell, its W
   cell. So a fork at a site has both child arms in one block: split and join fit one reaction, and the parent arrival is the
   other block. A fork one cell off that parity has its arms split across cuts.
3. Free-fall layout. Left part straight down, right part east then down. Below any leaf its column is empty
   forever (wires crossing a column come only from ancestors above). With columns spaced 2 apart, the column beside a
   spine is free too. So a spine's nodes can slide down leaf first, each fork's right arm gaining a vertical piece in that free
   column while right parts never move. Stems above any node then get room with no whole-part shift.
   Fork slide, in one block: F{N,S,E} over a wire cell {N,S}, with E cell {W,E} and an empty corner becomes wire {N,S}
   over F, with E cell {S,E} and corner {W,N}.
4. Kink: a 2-cell slack (a U in a wire) moves along a straight run in one reaction if the next cells beside the
   run are empty, and absorbs at a corner.
5. Width is the unsolved part: `unit` needs a new leaf beside a wire. A leaf in a narrow gap has no free column.
6. Cross needs no crossing wires: swap is a chirality bit on a fork, so wires are planar and the left/right reading is
   not, and cross = rot ; second(swap) ; rot⁻¹. Rotation in one block when two forks are adjacent:
   O{N,E,S} over I{N,E,S}, with r{W,E} and v{W,E} beside them, becomes O'{N,E,S} over wire {N,S}, with I'{W,E,S} over {N,E}.
   moves_only allows rot for this reason. A wire-crossing cell is also possible: a cell holding two strands, with one
   reaction switching it between the two smoothings and the plus.
7. Program as matter, one walker. Keep a route: a path through the program's nodes, the link and the data's nodes, each fork on it holding a
   routed-to field so it acts as a wire. One token runs along the route; each end grows or shrinks it. Going up needs no marks: at a
   fork the arm it came from is known by geometry. The head's direction bit flips at †. A stuck run can reflect: flip the
   direction and walk back, undoing everything, which gives `refuses` for one reaction. Time is (steps × route length).
8. Recursion: fix re-enters nodes already on the route, so the route between the fix and the call must be saved on a
   stack. A stack can be a growing strand off the link, in free space, that the reading ignores: push each breadcrumb as a
   token, rebuild on return.
9. A flat stream of the program in Polish order can't skip a subprogram with finite cells (needs a depth counter),
   so a stream design has to get structure from somewhere. Far-apart program and data can't run two heads
   without tags of unbounded length (token trains), since a token must carry which branch it serves at each level.
10. A bijection can't reach a fixed point from another state, so a run stops only because what moved left. One head goes in; one head comes out.
11. A reaction's variables stand for whole fields of equal range. Two independent links in one block need their own product
    reactions, since a block is rewritten once per tick.

## Report back (under 40 lines, plain)
- The design in a few sentences: cells, what a token is, how each move works, where room comes from, how the program runs.
- `laws.py` output for your best design (rungs, counts, graded, speed), pasted.
- What you tried and dropped, and why (this is the search record, so include the numbers).
- Which laws hurt, and any you think are wrong. Any harness bugs you fixed.
- The path of your best design. Save your run output in `<family>/REPORT.txt` too.
