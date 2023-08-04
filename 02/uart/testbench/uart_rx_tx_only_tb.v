// Testbench to exercise both the UART Tx and Rx.
`include "core/uart/uart_rx_tx_only.v"
 
module uart_tb #(parameter PERIOD = 10);
    // Testbench with a 10 MHz internal clock.
    //
    // Interface with 115200 baud rate for UART
    // FINAL_VALUE = (10000000 / 115200 * 16) - 1 = 53 clocks per bit.
    //
    parameter CLOCK_PERIOD_NS = 10;
    parameter FINAL_VALUE     = 53;
       
    // Inputs
	reg clk;
	reg reset_n;
    
	reg tx_start;
	reg [7:0] tx_din;
	reg rx;

	// Outputs
	wire tx;
    wire tx_done_tick;
	wire [7:0] rx_dout;
    wire rx_done_tick;

    uart_rx_tx_only uart_driver (
        .clk(clk),
        .reset_n(reset_n),
        
        .tx_din(tx_din),
        .tx_start(tx_start),
        .tx_done_tick(tx_done_tick),
        .tx(tx),

        .rx(rx),
        .rx_done_tick(rx_done_tick),
        .rx_dout(rx_dout),
        .timer_final_value(11'd53)
    );
    
    initial 
    begin 
        $display("Starting the UART Simulation");
        clk = 0; 
        reset_n = 0;
        tx_start = 0;
    end 

    always #CLOCK_PERIOD_NS clk = ~clk;
    
    //setup loopback 
    always @ (tx) rx = tx;
        initial begin 
            // Wait 100 ns for global reset to finish
            #100; 
            reset_n = 1;

            // Send data using tx portion of UART and wait until data is recieved.
            tx_start = 1;
            tx_din = 8'b0111_1110;
            
            // Wait until the receiver sets signal to done.
            wait (rx_done_tick == 1)
            // Read out the signal.
            if (rx_dout == 8'b0111_1110)
                $display("Success.");
            else
                $display("Failure : rx %b - tx %b", rx_dout, tx_din);

        $finish;
    end

endmodule
