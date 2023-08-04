
module clock_divider (
    input wire clk, 
    output reg o_clk
);
    parameter DIVISOR = 28'd2;

    always @(posedge clock_in) begin
        counter <= counter + 28'd1;
        if(counter>=(DIVISOR - 1))
            counter <= 28'd0;
        o_clk <= (counter < DIVISOR / 2) ? 1'b1 : 1'b0;
    end
endmodule

