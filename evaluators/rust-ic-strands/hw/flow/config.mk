# OpenROAD-flow-scripts design: one block unit (8 sites of storage and their turn logic) on IHP SG13G2.
# HW is set by layout.sh to the hw/ directory.
export DESIGN_NAME = strands_block
export DESIGN_NICKNAME = strands_block
export PLATFORM = ihp-sg13g2
export VERILOG_FILES = $(HW)/rtl/strands_block.v $(HW)/rtl/strands_stages.v
export VERILOG_INCLUDE_DIRS = $(HW)/rtl
export SDC_FILE = $(HW)/flow/strands_block.sdc
# The platform reads the period out of the constraints' text, which here is a variable.
export ABC_CLOCK_PERIOD_IN_PS = $(shell echo $$(( $(CLOCK_PERIOD_NS) * 1000 )))
export CORE_UTILIZATION ?= 35
export PLACE_DENSITY ?= 0.5
# Room beside every cell while placing, so pins on Metal2 stay reachable; none once legalized, so
# the diodes that fix long wires' antenna effects still fit.
export CELL_PAD_IN_SITES_GLOBAL_PLACEMENT = 1
export CELL_PAD_IN_SITES_DETAIL_PLACEMENT = 0
# IHP's current routing directions and tracks (layout.sh hands make the flipped technology LEF and
# the pin layers to match, which the platform does not let a design set) and power grid.
export MAKE_TRACKS = $(HW)/flow/make_tracks.tcl
export PDN_TCL = $(HW)/flow/pdn.tcl
# Each stage is mapped on its own, in one area-minded pass: the default flattens everything and
# runs ABC's speed script five rounds deep, hours on this design.
export SYNTH_HIERARCHICAL = 1
export ABC_AREA = 1
export SYNTH_SCRIPT = $(HW)/flow/synth.tcl
# OpenROAD's antenna repair does not converge here (58,000 diodes over five rounds, the same 1,200
# nets failing after each); IHP's own antenna deck judges the result instead (layout.sh).
export SKIP_ANTENNA_REPAIR = 1
# Pins stay where synthesis put them, so the netlist written for LVS is the routed one.
export SKIP_PIN_SWAP = 1
