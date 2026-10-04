# One clock. The turn logic is combinational from the block's registers to the next state, so the
# period is set by the deepest move (a step or an exchange, a rewrite's apply).
create_clock -name clk -period $::env(CLOCK_PERIOD_NS) [get_ports clk]
set_input_delay  [expr 0.2 * $::env(CLOCK_PERIOD_NS)] -clock clk [delete_from_list [all_inputs] [get_ports clk]]
set_output_delay [expr 0.2 * $::env(CLOCK_PERIOD_NS)] -clock clk [all_outputs]
