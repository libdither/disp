# What is left for the chip

The logic is done and checked (`validate.sh`). What is left is getting one block unit through
layout and the checks after it, then the parts around it.

## 1. Detailed routing does not finish

Everything before it works: synthesis (87,000 cells), placement, clock tree (22 ns slack at
50 ns), global routing (no congestion, 11 m of wire). The detailed router's first pass then
leaves 160,000–190,000 violations, about 80% of them Metal2 shorts and spacing errors, and its
repair passes stall:

| run | first pass | after pass 1 | after pass 2 |
|---|---|---|---|
| IHP's old directions (Metal2 horizontal), 35% use | 163,360 | 100,859 | 94,679 |
| IHP's current directions and power grid (`flip_directions.py`, `pdn.tcl`) | 172,027 | (cut off at 157,667, 40% through) | |

Tried along the way, each kept in `flow/` with its reason: per-stage synthesis with ABC's newer
mapper and no ABC buffering (ABC's own buffering added 50,000 cells), Metal5's real track pitch
(`make_tracks.tcl`; the pinned flow gave it a seventh of its tracks, which was why global routing
failed), 30–40% use, a site of padding around cells while placing, no antenna repair (see 2).

Not known yet: what the Metal2 shorts are between. Next steps, cheapest first:

- Read the router's markers (`reports/.../5_route_drc.rpt`, written every 5 passes, so let one
  run go that far), or route a single stage (`strands_move` as its own top) to see whether it is
  the design or the setup.
- Derate Metal2 in global routing (a `FASTROUTE_TCL` with `set_global_routing_layer_adjustment
  Metal2 0.5`) so long wires leave it to pin access.
- Move to OpenROAD and OpenROAD-flow-scripts 26Q2, whose IHP platform has new cell and via
  definitions as well as the new directions. nixpkgs has OpenROAD 26Q2, but its or-tools is
  marked broken, so the binary cache does not have it: it needs building, with the broken flag
  waived. `eda.sh` pins everything by revision, so this is a change of three lines there.
- If the current directions do not help, take `flip_directions.py` and `pdn.tcl` back out.

## 2. Antenna rules

OpenROAD's antenna repair does not converge on this design (58,000 diodes over five rounds, the
same 1,200 nets failing after each), so it is off (`SKIP_ANTENNA_REPAIR`) and IHP's antenna
deck judges the routed layout instead (`layout.sh`). It has not run on a finished layout yet;
expect violations on long Metal4/Metal5 wires. A repair that converges (newer OpenROAD), or
diodes placed by hand on the nets the deck names, would fix them.

## 3. After routing

- Timing: 2 ns of slack at 50 ns once global routing's wires count, so detailed routing may
  push it over. The longest paths run through the move stage's hop and the read ports;
  registering the read ports' outputs (a cycle more per stage that reads a neighbour) or a
  60 ns clock would give room.
- Layout versus schematic and the main design rules have only been run clean on OpenROAD's
  example design, never on the block unit.
- Metal density rules need fill, which belongs to a whole chip, not one tile.

## 4. Beyond one block unit

- The lattice around the block units (shifting the contents by the clock's offset, the pulse
  phase) is simulated clock by clock (`sim/tb_lattice.cpp`) but not laid out. It would be laid
  out with the block unit as a macro: per site it is a few multiplexers and the pulse logic.
- Size: 87,000 cells for 8 sites. The block's own storage and ports are 40,600 of them, the
  move stage 20,500, collection 8,200, the rewrite's setup 5,600. Doing a move as a short
  sequence of single switchboard writes, one per cycle, would shrink the stages a lot for a few
  more cycles per turn.
- Speed: a clock takes 50–80 cycles on average but up to 1,100, when a rewrite with five or six
  fresh agents tries its 1,024 seatings per square one per cycle. Trying several seatings per
  cycle would cut that worst case.
