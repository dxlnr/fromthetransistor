// Testbench to exercise both the UART Tx and Rx.
`include "core/uart/uart.v"
 
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
    
    reg rd_uart;
	reg wr_uart;
	reg [7:0] w_data;
	reg rx;

	// Outputs
	wire tx;
    wire rx_empty;
	wire [7:0] r_data;
    wire tx_full;

    uart uut (
        .clk(clk),
        .reset_n(reset_n),
        
        .w_data(w_data),
        .wr_uart(wr_uart),
        .tx_full(tx_full),
        .tx(tx),

        .rx(rx),
        .rd_uart(rd_uart),
        .rx_empty(rx_empty),
        .r_data(r_data),

        .timer_final_value(11'd53)
    );
    
    initial 
    begin 
        $display("Starting the UART Simulation");
        clk = 0; 
        reset_n = 0;
    end 

    always #CLOCK_PERIOD_NS clk = ~clk;
    
    //setup loopback 
    always @ (tx) rx = tx;
        initial begin 
            // Wait 100 ns for global reset to finish
            #100; 
            reset_n = 1;

            // Send data using tx portion of UART and wait until data is recieved.
            wr_uart = 1;
            w_data = 8'b0111_1110;
            
            // Wait until the receiver sets signal to done.
            wait(uut.recv.rx_done_tick == 1)
            #200000
            rd_uart = 1;
            wait(rx_empty == 1);

            // Read out the signal.
            if (r_data == 8'b0111_1110)
                $display("Success.");
            else
                $display("Failure : r_data %b - w_data %b", r_data, w_data);

        $finish;
    end

endmodule
