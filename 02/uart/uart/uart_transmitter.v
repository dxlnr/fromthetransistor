// UART Transmitter
//
module uart_transmitter
    #(parameter D_BITS=8,           // Data bits that are sent/received.
                SB_TICK=16
    )(
    input clk,                      // Top level system clock input.
    input reset_n,                  // Asynchronous active low reset.
    input s_tick,                   // Send the data on data pin.
    input [D_BITS -1:0] tx_din,     // Data input that gets sent out.
    input tx_start,                 // Data is available for fetching from FIFO.  
    output tx,                      // UART transmitter Pin.
    output reg tx_done_tick         // Set when transmitting is finished. 
    );
    
    reg [2:0] tx_state, tx_state_next;
    reg [3:0] s_register, s_register_next;
    reg [$clog2(D_BITS) - 1:0] n_bits_register, n_bits_register_next;
    reg [D_BITS - 1:0] b_bits_register, b_bits_register_next;
    reg tx_register, tx_register_next;

    // Define the FSM states.
    localparam t_state_idle = 0;
    localparam t_state_start = 1;
    localparam t_state_data = 2;
    localparam t_state_stop = 3;
    
    // initiate states registers 
    always @ (posedge clk, negedge reset_n) begin 
        if (!reset_n) begin 
            tx_state <= 0;
            s_register <= 0;
            n_bits_register <= 0;
            b_bits_register <= 0;
            tx_register <= 1'b1;
        end else
        begin
            tx_state <= tx_state_next;
            s_register <= s_register_next;
            n_bits_register <= n_bits_register_next;
            b_bits_register <= b_bits_register_next;
            tx_register <= tx_register_next;

        end
    end

    // next state logic section 
    always @ (*) 
    begin 
        tx_state_next = tx_state;
        s_register_next = s_register;
        n_bits_register_next = n_bits_register;
        b_bits_register_next = b_bits_register;
        tx_done_tick = 1'b0;
        case (tx_state) 
            t_state_idle:
            begin
                tx_register_next = 1'b1;
                if (!tx_start)
                    tx_state_next = t_state_idle;
                else begin
                    s_register_next =  0;
                    b_bits_register_next = tx_din;;
                    tx_state_next = t_state_start;
                end
            end
            t_state_start: 
            begin
                tx_register_next = 1'b0;
                if (!s_tick)
                    tx_state_next = t_state_start;
                else begin 
                    if (s_register == 15) begin 
                        s_register_next = 0;
                        n_bits_register_next = 0;
                        tx_state_next = t_state_data;
                    end
                    else begin 
                        s_register_next = s_register + 1;
                        tx_state_next = t_state_start;
                    end
                end
            end
            t_state_data: 
            begin
                tx_register_next = b_bits_register[0];
                if (!s_tick)
                    tx_state_next = t_state_data;
                else begin 
                    if (s_register == 15) begin 
                        s_register_next = 0;
                        b_bits_register_next = {1'b0, b_bits_register[D_BITS - 1:1]};
                        if (n_bits_register == D_BITS -1)
                            tx_state_next = t_state_stop;
                        else begin 
                            n_bits_register_next = n_bits_register + 1;
                            tx_state_next = t_state_data;
                        end
                    end 
                    else begin 
                       s_register_next = s_register + 1;
                       tx_state_next = t_state_data;
                    end
                end
            end
            t_state_stop: 
            begin 
                tx_register_next = 1'b1;
                if (!s_tick)
                    tx_state_next = t_state_stop;
                else begin 
                    if (s_register == SB_TICK -1) begin 
                        tx_done_tick = 1'b1;
                        tx_state_next = t_state_idle;
                    end 
                    else begin 
                        s_register_next = s_register + 1;
                        tx_state_next = t_state_stop;
                    end
                end
            end
            default: tx_state_next = t_state_idle;
        endcase
    end
    
    // output logic section
    assign tx = tx_register;

endmodule
