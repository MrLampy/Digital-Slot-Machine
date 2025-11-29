gui_open_window Wave
gui_sg_create clk25_group
gui_list_add_group -id Wave.1 {clk25_group}
gui_sg_addsignal -group clk25_group {clk25_tb.test_phase}
gui_set_radix -radix {ascii} -signals {clk25_tb.test_phase}
gui_sg_addsignal -group clk25_group {{Input_clocks}} -divider
gui_sg_addsignal -group clk25_group {clk25_tb.CLK_IN1}
gui_sg_addsignal -group clk25_group {{Output_clocks}} -divider
gui_sg_addsignal -group clk25_group {clk25_tb.dut.clk}
gui_list_expand -id Wave.1 clk25_tb.dut.clk
gui_sg_addsignal -group clk25_group {{Status_control}} -divider
gui_sg_addsignal -group clk25_group {clk25_tb.RESET}
gui_sg_addsignal -group clk25_group {clk25_tb.LOCKED}
gui_sg_addsignal -group clk25_group {{Counters}} -divider
gui_sg_addsignal -group clk25_group {clk25_tb.COUNT}
gui_sg_addsignal -group clk25_group {clk25_tb.dut.counter}
gui_list_expand -id Wave.1 clk25_tb.dut.counter
gui_zoom -window Wave.1 -full
