// UART Receiver 
//
module uart_receiver 
    #(parameter D_BITS=8,               // Data bits that are sent/received.
                SB_TICK=16
    )(
    input clk,                          // Top level system clock input.
    input reset_n,                      // Asynchronous active low reset.
    input rx,                           // UART receiver Pin.
    input s_tick,                       // Receive the data on data pin.
    output [D_BITS - 1:0] rx_dout,      // Data output that gets sent to FIFO.
    output reg rx_done_tick             // Set when receiving is finished.
    );
    
    reg [2:0] rx_state, rx_state_next;
    reg [3:0] s_register, s_register_next;
    reg [$clog2(D_BITS) - 1:0] n_bits_register, n_bits_register_next;
    reg [D_BITS - 1:0] b_bits_register, b_bits_register_next;

    // Define the FSM states.
    localparam r_state_idle = 0;
    localparam r_state_start = 1;
    localparam r_state_data = 2;
    localparam r_state_stop = 3;

    // initiate state registers
    always @ (posedge clk, negedge reset_n) begin 
        if (!reset_n) 
        begin
            rx_state <= 0;
            s_register <= 0;
            n_bits_register <= 0;
            b_bits_register <= 0;
        end else 
        begin
            rx_state <= rx_state_next;
            s_register <= s_register_next;
            n_bits_register <= n_bits_register_next;
            b_bits_register <= b_bits_register_next;
        end
    end
    
    // next state logic section
    always @ (*) begin
        rx_state_next = rx_state;
        s_register_next = s_register;
        n_bits_register_next = n_bits_register;
        b_bits_register_next = b_bits_register;
        rx_done_tick = 1'b0;
        case(rx_state)
            r_state_idle: 
                if (rx == 0)
                begin
                    s_register_next = 0;
                    rx_state_next = r_state_start;
                end else
                    rx_state_next = r_state_idle;
            r_state_start: 
                if (!s_tick)
                    rx_state_next = r_state_start;
                else begin
                    if (s_register == 7) begin
                        s_register_next = 0;
                        n_bits_register_next = 0;
                        rx_state_next = r_state_data;
                    end 
                    else begin 
                        s_register_next = s_register + 1;
                        rx_state_next = r_state_start;
                    end
                end
            r_state_data: 
                if (!s_tick)
                    rx_state_next = r_state_data;
                else begin
                    if (s_register == 15)
                    begin
                       s_register_next = 0;
                       b_bits_register_next = {rx, b_bits_register[D_BITS - 1:1]};

                       if (n_bits_register == D_BITS - 1)
                           rx_state_next = r_state_stop;
                       else begin 
                           n_bits_register_next = n_bits_register + 1;
                       end
                    end
                    else begin
                        s_register_next = s_register + 1;
                        rx_state_next = r_state_data;
                    end
                end
            r_state_stop: 
                if (!s_tick)
                    rx_state_next = r_state_stop;
                else begin 
                    if (s_register == SB_TICK - 1) begin
                        rx_done_tick = 1'b1;
                        rx_state_next = r_state_idle;
                    end 
                    else begin 
                        s_register_next = s_register + 1;
                        rx_state_next = r_state_stop;
                    end
                end
            default: rx_state_next = r_state_idle;
        endcase
    end

    // output logic section
    assign rx_dout = b_bits_register;
    
endmodule
