# OpenROAD-flow-scripts' synthesis (scripts/synth.tcl), mapping with abc_area.script from here:
# ABC's older mapper (map), which the flow's script uses, crashes on the move stage whenever
# structural choices are present (yosys-abc of Yosys 0.55). This maps the way Yosys does by default
# (choices in fast mode, the newer mapper &nf) minus its SAT sweep (&fraig, which runs for hours on
# the block's storage). It leaves buffering and sizing to OpenROAD after placement: ABC's own
# (buffer, upsize) fills the read ports' select lines with chains of 50,000 small buffers.
set ::abc_area_script [file join [file dirname [file normalize [info script]]] abc_area.script]
trace add variable ::abc_args write {apply {{args} {
  set i [lsearch -exact $::abc_args -script]
  if {$i >= 0} { lset ::abc_args [expr {$i + 1}] $::abc_area_script }
}}}
source $::env(SCRIPTS_DIR)/synth.tcl
