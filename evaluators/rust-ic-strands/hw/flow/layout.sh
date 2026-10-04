#!/usr/bin/env bash
# Lay out one block unit on IHP SG13G2 and check the layout independently of the flow that made it:
#   place and route       OpenROAD-flow-scripts: synthesis, floorplan, placement, clock tree, routing
#   design rules          IHP's KLayout deck (main tables) on the final GDS, after drawing metal under
#                         the pins (the flow writes block pins as pin shapes only)
#   layout vs schematic   IHP's KLayout deck, comparing the final GDS with the routed netlist, fillers
#                         included, the standard cells' interchangeable inputs declared
#   timing                the flow's final static timing at CLOCK_PERIOD_NS
# OUT=dir for the work files; CLOCK_PERIOD_NS (default 50), CORE_UTILIZATION, PLACE_DENSITY pass through.
set -euo pipefail
HW=$(cd "$(dirname "$0")/.." && pwd)
WORK=${OUT:-${XDG_CACHE_HOME:-$HOME/.cache}/rust-ic-strands-hw}/layout
export HW WORK CLOCK_PERIOD_NS=${CLOCK_PERIOD_NS:-50}
mkdir -p "$WORK"
exec systemd-run --user --scope -q -p MemoryMax=${LAYOUT_MEM:-16G} -p MemorySwapMax=2G timeout ${LAYOUT_TIMEOUT:-86400} \
  "$HW/eda.sh" bash -c '
set -euo pipefail
ORFS=$EDA_CACHE/orfs/flow
PLAT=$ORFS/platforms/ihp-sg13g2
DECK=$EDA_CACHE/ihp/ihp-sg13g2/libs.tech/klayout/tech
R=$WORK/results/ihp-sg13g2/strands_block/base
step() { echo "   $1"; }

step "place and route ($WORK/logs)"
make -C "$ORFS" --no-print-directory DESIGN_CONFIG="$HW/flow/config.mk" WORK_HOME="$WORK" \
  YOSYS_EXE=$(which yosys) OPENROAD_EXE=$(which openroad) KLAYOUT_CMD=$(which klayout) > "$WORK/flow.log" 2>&1 \
  || { tail -20 "$WORK/flow.log"; exit 1; }

step "design rules"
klayout -b -r "$HW/flow/pins_to_metal.py" -rd src="$R/6_final.gds" -rd dst="$WORK/final.gds" > /dev/null
klayout -b -r "$DECK/drc/ihp-sg13g2.drc" -rd input="$WORK/final.gds" -rd topcell=strands_block \
  -rd report="$WORK/drc.lyrdb" -rd threads=$(nproc) > "$WORK/drc.log" 2>&1
python3 - "$WORK/drc.lyrdb" <<PY
import sys, collections, xml.etree.ElementTree as ET
items = ET.parse(sys.argv[1]).getroot().find("items")
c = collections.Counter(i.find("category").text for i in items)
print("     %d violations %s" % (sum(c.values()), dict(c.most_common(5)) if c else ""))
sys.exit(1 if c else 0)
PY

step "layout versus schematic"
{ cat "$PLAT/cdl/sg13g2_stdcell.cdl"; printf ".SUBCKT sg13g2_fill_1 VDD VSS\n.ENDS\n.SUBCKT sg13g2_fill_2 VDD VSS\n.ENDS\n"; } > "$WORK/masters.cdl"
cat > "$WORK/cdl.tcl" <<TCL
read_db $R/6_final.odb
add_global_connection -net VDD -pin_pattern {^VDD\$} -power
add_global_connection -net VSS -pin_pattern {^VSS\$} -ground
global_connect
write_cdl -masters $WORK/masters.cdl -include_fillers $WORK/design.cdl
TCL
openroad -exit "$WORK/cdl.tcl" > "$WORK/cdl.log" 2>&1
cat "$WORK/masters.cdl" "$WORK/design.cdl" > "$WORK/strands_block.cdl"
# IHP deck, with the interchangeable inputs of every standard cell declared just before comparing.
python3 "$HW/flow/equivalent_pins.py" "$PLAT/lib/sg13g2_stdcell_typ_1p20V_25C.lib" > "$WORK/equivalent_pins.lvs"
python3 - "$DECK/lvs/sg13g2.lvs" "$WORK/equivalent_pins.lvs" "$DECK/lvs/sg13g2_rust_ic_strands.lvs" <<PY
import sys
deck, eq, out = open(sys.argv[1]).read(), open(sys.argv[2]).read(), sys.argv[3]
at = "  logger.info(\x27Starting SG13G2 LVS Alignment\x27)\n  align\n"
assert deck.count(at) == 1, "the LVS deck changed shape"
open(out, "w").write(deck.replace(at, at + "".join("  " + l + "\n" for l in eq.splitlines())))
PY
klayout -b -r "$DECK/lvs/sg13g2_rust_ic_strands.lvs" -rd input="$WORK/final.gds" -rd schematic="$WORK/strands_block.cdl" \
  -rd topcell=strands_block -rd run_mode=deep -rd report="$WORK/lvs.lvsdb" -rd log="$WORK/lvs.log" > /dev/null 2>&1 || true
grep -q "Netlists match" "$WORK/lvs.log" && echo "     netlists match" || { echo "     netlists differ (see $WORK/lvs.log)"; exit 1; }

step "timing and area"
grep -E "^(tns|wns|worst slack)|design area|Design area" "$WORK/reports/ihp-sg13g2/strands_block/base/6_finish.rpt" "$WORK/logs/ihp-sg13g2/strands_block/base/6_report.log" 2>/dev/null | sed "s|^.*/||" | head -6 | sed "s/^/     /"
'
