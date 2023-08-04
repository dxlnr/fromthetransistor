// Baud Rate Generator 
//
// The UART Baud Rate Generator (BRG) is a timer used to generate 
// the clocking mechanism required for communication. 
// The BRG generates 16 clock periods per data bit.
//
module baud_rate_generator 
    #(parameter BITS = 4
    )(
    input clk,                              // Top level system clock input. 
    input reset_n,                          // Asynchronous active low reset.
    input ena,                              // Enable Bit
    input [BITS - 1:0] timer_final_value,   // Counts up to this value. 
    output done                             // Signal set when final value is reached.
    );

    reg [BITS - 1:0] count_register, count_register_next;

    always @ (posedge clk, negedge reset_n) begin 
        if (!reset_n)
            count_register <= 'b0;
        else if(ena)
            count_register <= count_register_next;
        else 
            count_register <= count_register;
    end

    assign done = (count_register == timer_final_value);

    always @ (*)
        count_register_next = done ? 'b0 : count_register + 1;
endmodule


