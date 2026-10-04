# IHP's routing directions as the PDK has them now (OpenROAD-flow-scripts 26Q2 carries them): Metal1
# and every other layer up run horizontally, Metal2 and every other vertically, each at the pitch of
# its direction. The pinned flow's technology LEF has the old ones, the other way round, which puts
# Metal2 along the cell rows where it must also reach every pin.
#   python flip_directions.py in.lef out.lef
import re, sys
direction = {"Metal1": "HORIZONTAL", "Metal2": "VERTICAL", "Metal3": "HORIZONTAL", "Metal4": "VERTICAL",
             "Metal5": "HORIZONTAL", "TopMetal1": "VERTICAL", "TopMetal2": "HORIZONTAL"}
pitch = {"HORIZONTAL": "0.42", "VERTICAL": "0.48"}
out, layer, flipped = [], None, 0
for line in open(sys.argv[1]):
    m = re.match(r"LAYER\s+(\S+)", line)
    if m: layer = m.group(1)
    elif re.match(r"END\s+" + re.escape(layer or "-"), line): layer = None
    elif layer in direction and re.match(r"\s*DIRECTION\b", line):
        line = re.sub(r"(HORIZONTAL|VERTICAL)", direction[layer], line); flipped += 1
    elif layer in direction and layer.startswith("Metal") and re.match(r"\s*PITCH\b", line):
        line = re.sub(r"[\d.]+", pitch[direction[layer]], line, count=1)
    out.append(line)
assert flipped == len(direction), "the LEF changed shape"
open(sys.argv[2], "w").writelines(out)
