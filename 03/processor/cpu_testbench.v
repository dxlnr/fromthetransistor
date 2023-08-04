`define hdl_path_regf c.regs

module cpu_testbench #(parameter PERIOD = 10);
    parameter CLOCK_PERIOD_NS = 5;

	reg clk;
    reg reset_n;
    wire trap;

    processor p (
        .clk(clk),
        .reset_n(reset_n),
        .trap(trap)
    );

    initial begin 
        string firmware;
        clk = 0; 
        reset_n = 0;
        $display("Running ARM Processor Testbench.");
        if ($value$plusargs("firmware=%s", firmware)) begin
            $display($sformatf("  Reading from %s", firmware));
        end else begin
            $display($sformatf("  Expecting an argument %s", firmware), "ERROR");
            $finish;
        end
        $readmemh(firmware, p.fs.im.mem);
    end 

    always #CLOCK_PERIOD_NS clk = ~clk;

    always @(posedge clk) begin
        reset_n <= 1;
        $display("pc:%d -- ins:%h -- alu_c:%b alu_res:%b wb_data:%b -- wb:%b mem_r:%b mem_w:%b",
            p.pc,
            p.w_fs_ins,
            p.w_de_alu_c,
            p.w_eu_alu_res,
            p.w_wb_wb_data,
            p.w_wb_wb_en,
            p.w_eu_mem_r_en,
            p.w_eu_mem_w_en
        );
    end

    always @(posedge trap) begin
        $display("TRAP. finished.");
        $finish;
    end

    initial begin
        #500
        $display("\nfinished.");
        $finish;
    end
endmodule
