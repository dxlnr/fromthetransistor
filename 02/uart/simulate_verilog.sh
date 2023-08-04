#!/bin/bash -e 

echo "TESTBENCH : UART without FIFO"
iverilog -Wall -o tb_uart_rx_tx_only testbench/uart_rx_tx_only_tb.v && vvp tb_uart_rx_tx_only
echo ""
echo "TESTBENCH : UART"
iverilog -Wall -o tb_uart testbench/uart_tb.v && vvp tb_uart

# Clean up
rm -rf tb_uart_rx_tx_only
rm -rf tb_uart
