# OpenROAD-flow-scripts design: one block unit (8 sites of storage and their turn logic) on IHP SG13G2.
# HW is set by layout.sh to the hw/ directory.
export DESIGN_NAME = strands_block
export DESIGN_NICKNAME = strands_block
export PLATFORM = ihp-sg13g2
export VERILOG_FILES = $(HW)/rtl/strands_block.v $(HW)/rtl/strands_stages.v
export VERILOG_INCLUDE_DIRS = $(HW)/rtl
export SDC_FILE = $(HW)/flow/strands_block.sdc
export CORE_UTILIZATION ?= 40
export PLACE_DENSITY ?= 0.55
# Pins stay where synthesis put them, so the netlist written for LVS is the routed one.
export SKIP_PIN_SWAP = 1
