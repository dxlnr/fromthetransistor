`include "core/uart/uart_transmitter.v"
`include "core/uart/uart_receiver.v"
`include "core/uart/baud_rate_generator.v"
`include "core/uart/fifo_buffer.v"

// UART
//
// Combine receiver, transmitter, baud rate generator and fifo buffers.
// Able to operate 8 bits of serial data, one start bit, one stop bit.
//
module uart 
    #(parameter D_BITS=8,
                SB_TICK=16
    )(
    input clk,
    input reset_n,
    
    // transmitter port
    input [D_BITS - 1:0] w_data,
    input wr_uart,
    output tx_full,
    output tx,

    // receiver port
    input rx,
    input rd_uart,
    output rx_empty,
    output [D_BITS - 1:0] r_data,

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
    wire rx_done_tick;
    wire [D_BITS - 1: 0] rx_dout;
    uart_receiver #(.D_BITS(D_BITS), .SB_TICK(SB_TICK)) recv (
        .clk(clk),
        .reset_n(reset_n),
        .rx(rx),
        .s_tick(s_tick),
        .rx_done_tick(rx_done_tick),
        .rx_dout(rx_dout)
    );

    fifo_buffer #(.D_BITS(D_BITS), .BYTE_SIZE(8)) fifo_rx (
        .clk(clk),
        .reset_n(reset_n),
        .din(rx_dout),
        .wr_en(rx_done_tick),
        .rd_en(rd_uart),
        .dout(r_data),
        .full(),
        .empty(rx_empty)
    );

    // UART transmitter
    wire tx_fifo_empty; 
    wire tx_done_tick;
    wire [D_BITS - 1: 0] tx_din;
    uart_transmitter #(.D_BITS(D_BITS), .SB_TICK(SB_TICK)) trans (
        .clk(clk),
        .reset_n(reset_n),
        .tx_start(~tx_fifo_empty),
        .s_tick(s_tick),
        .tx_din(tx_din),
        .tx_done_tick(tx_done_tick),
        .tx(tx)
    );

    fifo_buffer #(.D_BITS(D_BITS), .BYTE_SIZE(8)) fifo_tx (
        .clk(clk),
        .reset_n(reset_n),
        .din(w_data),
        .wr_en(wr_uart),
        .rd_en(tx_done_tick),
        .dout(tx_din),
        .full(tx_full),
        .empty(tx_fifo_empty)
    );

endmodule
