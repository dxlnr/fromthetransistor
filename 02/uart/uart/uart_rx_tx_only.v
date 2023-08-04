`include "core/uart/uart_transmitter.v"
`include "core/uart/uart_receiver.v"
`include "core/uart/baud_rate_generator.v"

// UART
//
// Combine receiver, transmitter and baud rate generator.
// Able to operate 8 bits of serial data, one start bit, one stop bit.
//
module uart_rx_tx_only 
    #(parameter D_BITS=8,
                SB_TICK=16
    )(
    input clk,
    input reset_n,
    
    // transmitter port
    input [D_BITS - 1:0] tx_din,
    input tx_start,
    output tx_done_tick,
    output tx,

    // receiver port
    input rx,
    output rx_done_tick,
    output [D_BITS - 1:0] rx_dout,

    // baud rate generator
    input [10:0] timer_final_value
    );
    
    // Baud Rate Generator
    wire s_tick;

    baud_rate_generator #(.BITS(11)) brg (
        .clk(clk),
        .reset_n(reset_n),
        .ena(1'b1),
        .timer_final_value(timer_final_value),
        .done(s_tick)
    );

    // UART Receiver
    uart_receiver #(.D_BITS(D_BITS), .SB_TICK(SB_TICK)) recv (
        .clk(clk),
        .reset_n(reset_n),
        .rx(rx),
        .s_tick(s_tick),
        .rx_done_tick(rx_done_tick),
        .rx_dout(rx_dout)
    );

    // UART transmitter
    uart_transmitter #(.D_BITS(D_BITS), .SB_TICK(SB_TICK)) trans (
        .clk(clk),
        .reset_n(reset_n),
        .tx_start(tx_start),
        .s_tick(s_tick),
        .tx_din(tx_din),
        .tx_done_tick(tx_done_tick),
        .tx(tx)
    );

endmodule
