// FIFO Data Buffer Module 
//
// Configurable in address size and data size.
// Serves as an intermediary for the uart protocol to allow a device with 
// different speed to communicate with the uart receiver and transmitter.
//
module fifo_buffer 
    #(parameter D_BITS=8,           // Data bits of single read and write.
                BYTE_SIZE=8         // Address bits define the size of the buffer.
    )(
    input clk,                      // Top level system clock input.
    input reset_n,                  // Asynchronous active low reset. 
    input [D_BITS - 1:0] din,       // Data in.
    input wr_en,                    // Write enable 
    input rd_en,                    // Read enable
    output [D_BITS - 1:0] dout,     // Data out
    output full,                    // Set when FIFO buffer is full.
    output empty                    // Set when FIFO buffer is empty.
    );    
    
    reg [3:0] head_ptr, tail_ptr;
    reg [3:0] counter;
    reg [D_BITS - 1:0] dout;
    reg empty, full;

    reg [D_BITS - 1:0] mem [0:BYTE_SIZE - 1];
    integer i;

    always @ (counter) begin 
        empty = (counter == 0);
        full = (counter == BYTE_SIZE);
    end

    always @ (posedge clk, negedge reset_n) begin 
        if (!reset_n) begin 
            head_ptr <= 0;
            tail_ptr <= 0;
            counter <= 0;
            dout <= 0;
            for (i = 0; i < BYTE_SIZE; i = i + 1) begin 
                mem[i] <= 0;
            end
        end else begin 
            if ((wr_en) && (counter < BYTE_SIZE)) begin 
                mem[head_ptr] <= din;
                counter <= counter + 1;
                head_ptr <= head_ptr + 1;
            end
            else if ((rd_en) && (counter > 0)) begin 
                dout <= mem[tail_ptr];
                counter <= counter - 1;
                tail_ptr <= tail_ptr + 1;
            end
            else begin
                counter <= counter;
                head_ptr <= head_ptr;
                tail_ptr <= tail_ptr;
                dout <= dout;
                mem[head_ptr] <= mem[head_ptr];
            end
        end
    end
    
endmodule
